#' @title Run Machine Learning Benchmarks as distributed Experiments
#' @description
#' This function is a very parallel version of \code{\link{benchmark}}.
#' Experiments are created in the provided registry for each combination of
#' learners, tasks and resamplings. The experiments are then stored in a registry and the 
#' runs can be started via \code{\link[batchtools]{submitJobs}]}. For details on the usage
#' and support backends have a look at the batchtools tutorial page: 
#' \url{https://github.com/mllg/batchtools}.
#' 
#' The general workflow with \code{\link{batchmark}} looks like that:
#' \itemize{
#' \item{1.}{\code{batchtools::createExperimentRegistry()}}
#' \item{2.}{\code{batchmark(...)}}
#' \item{3.}{\code{batchtools:submitJobs()}}
#' \item{3.5}{wait until the jobs are finished...}
#' \item{4.}{\code{reduceBatchtoolsResult()}}
#' }
#'
#' @param learners [(list of) \code{\link{Learner}}]\cr
#'   Learning algorithms which should be compared.
#' @param tasks [(list of) \code{\link{Task}}]\cr
#'   Tasks that learners should be run on.
#' @param resamplings [(list of) \code{\link{ResampleDesc}} | \code{\link{ResampleInstance}}]\cr
#'   Resampling strategy for each tasks.
#'   If only one is provided, it will be replicated to match the number of tasks.
#'   If missing, a 10-fold cross validation is used.
#' @param measures [(list of) \code{\link{Measure}}]\cr
#'   Performance measures for all tasks.
#'   If missing, the default measure of the first task is used.
#' @template arg_models
#' @param reg [\code{\link[batchtools]{Registry}}]\cr
#'   Registry, created by \code{\link[bacthtools]{makeExperimentRegistry}}. If not explicitly passed, 
#'   uses the last created registry.
#' @return [\code{data.table}]. Generated job ids are stored in the column \dQuote{job.id}.
#' @export
#' @family benchmark
batchmark = function(learners, tasks, resamplings, measures = NULL, models = TRUE, reg = batchtools::getDefaultRegistry()) {
  
  requirePackages("batchtools", why = "batchmark", default.method = "attach")
  assertList(learners, types = "Learner", min.len = 1L)
  learner.ids = vcapply(learners, "[[", "id")
  if (anyDuplicated(learner.ids))
    stop("Learners need unique ids!")
  assertList(tasks, types = "Task", min.len = 1L)
  task.ids = vcapply(tasks, getTaskId)
  if (anyDuplicated(task.ids))
    stop("Tasks need unique ids!")
  
  assertFlag(models)

  # check resamplings
  if (missing(resamplings)) {
    resamplings = replicate(length(tasks), makeResampleDesc("CV", iters = 10L), simplify = FALSE)
  } else if (inherits(resamplings, "ResampleInstance") || inherits(resamplings, "ResampleDesc")) {
    resamplings = replicate(length(tasks), resamplings, simplify = FALSE)
  } else {
    assertList(resamplings)
    if (length(resamplings) != length(tasks))
      stop("Number of resampling strategies and number of tasks differ!")
  }
  
  if (is.null(measures)) {
    measures = list(getDefaultMeasure(tasks[[1L]]))
  } else {
    assertList(measures, types = "Measure", min.len = 1L)
  }

  reg$packages = union(reg$packages, "mlr")
  
  #set name of learner list
  lrn.ids = vapply(learners, getLearnerId, FUN.VALUE = character(1))
  names(learners) = lrn.ids

  # generate problems
  pdes = Map(function(id, task, rdesc, seed) {
    batchtools::addProblem(id, data = list(rdesc = rdesc, task = task, measures = measures, learners = learners), fun = resample.fun, seed = seed, reg = reg)
    data.table(i = seq_len(rdesc$iters))
  }, id = task.ids, task = tasks, rdesc = resamplings, seed = reg$seed + seq_along(tasks))

  # generate algos
  ades = Map(function(id, learner) {
    apply.fun = getAlgoFun(learner, measures, models)
    batchtools::addAlgorithm(id, apply.fun, reg = reg)
    data.table()
  }, id = learner.ids, learner = learners)

  # add experiments
  batchtools::addExperiments(reg = reg, prob.designs = pdes, algo.designs = ades, repls = 1L)
}

resample.fun = function(job, data, i) {
  rin = makeResampleInstance(desc = data$rdesc, task = data$task)
  list(train = rin$train.inds[[i]], test = rin$test.inds[[i]], weights = rin$weights[[i]])
}

getAlgoFun = function(lrn, measures, models) {
  force(lrn)
  force(measures)
  force(models)
  function(job, data, instance) {

    extract.this = getExtractor(lrn)

    calculateResampleIterationResult(learner = lrn, task = data$task, train.i = instance$train, test.i = instance$test, 
      measures = measures, weights = instance$weights, rdesc = data$rdesc, model = models, extract = extract.this)
  }
}

#' @title Reduce results of a batch-distributed benchmark
#' @description
#' 
#' This creates a \code{\link{BenchmarkResult}} from a \code{\link[batchtools]{Registry}}.
#' To setup the benchmark have a look at \code{\link{batchmark}}.
#' 
#' @param ids [\code{\link[base]{data.frame}} or \code{integer}]\cr
#'   A \code{\link[base]{data.frame}} (or \code{\link[data.table]{data.table}})
#'   with a column named \dQuote{job.id}.
#'   Alternatively, you may also pass a vector of integerish job ids.
#'   If not set, defaults to all jobs.
#' @template arg_keep_pred
#' @param reg [\code{\link[batchtools]{Registry}}]\cr
#'   Registry, created by \code{\link[bacthtools]{makeExperimentRegistry}}. If not explicitly passed, 
#'   uses the last created registry.
#' @return [\code{\link{BenchmarkResult}}].
#' @export
#' @family benchmark
reduceBatchmarkResults = function(ids = NULL, keep.pred = TRUE, reg = batchtools::getDefaultRegistry()) {
  
  assertFlag(keep.pred)
  
  problems = batchtools::getProblemIds(reg)
  algorithms = batchtools::getAlgorithmIds(reg)
  
  result = lapply(problems, function(p) {
    problem = readRDS(paste0(reg$file.dir, "/problems/", p, ".rds"))
    rin = makeResampleInstance(problem$data$rdesc, problem$data$task)
    rr = lapply(algorithms, function(a) {
      res = batchtools::reduceResultsList(batchtools::findExperiments(prob.name = p, algo.name = a, ids = ids))
      models = !is.null(res[[1]]$model)
      lrn = problem$data$learner[[a]]
      extract.this = getExtractor(lrn)
      rs = mergeResampleResult(learner.id = a, task = problem$data$task, iter.results = res, measures = problem$data$measures, 
        rin = rin, keep.pred = keep.pred, models = models, show.info = FALSE, runtime = NA, extract = extract.this)
      rs$learner = lrn
      addClasses(rs, "ResampleResult")
    })
    setNames(rr, algorithms)
  })
  
  names(result) = problems
  
  problem = readRDS(paste0(reg$file.dir, "/problems/", problems[1], ".rds"))

  makeS3Obj(classes = "BenchmarkResult",
    results = result,
    measures = problem$data$measures,
    learners = problem$data$learner)
}




