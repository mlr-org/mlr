#' @title Run machine learning benchmarks as distributed experiments.
#' 
#' @description
#' This function is a very parallel version of \code{\link{benchmark}}.
#' Experiments are created in the provided registry for each combination of
#' learners, tasks and resamplings. The experiments are then stored in a registry and the 
#' runs can be started via \code{\link[batchtools]{submitJobs}]}. A job is one train/test split
#' of the outer resampling. In case of nested resampling (e.g. with \code{\link{makeTuneWrapper}}), 
#' each job is a full run of inner resampling, which can be parallelized in a second step 
#' with \pkg{ParallelMap}. For details on the usage and support backends have 
#' a look at the batchtools tutorial page: 
#' \url{https://github.com/mllg/batchtools}.
#' 
#' The general workflow with \code{batchmark} looks like that:
#' \enumerate{
#' \item{1.}{\code{batchtools::createExperimentRegistry()}}
#' \item{2.}{\code{batchmark(...)}}
#' \item{3.}{\code{batchtools:submitJobs()}}
#' \item{4.}{wait until the jobs are finished...}
#' \item{5.}{\code{reduceBatchtoolsResult()}}
#' }
#' @inheritParams benchmark
#' @param data.ids [\code{integer}]\cr
#'   Dataset IDs to download from Open-ML. Default is none.
#' @param resamplings [(list of) \code{\link{ResampleDesc}}]\cr
#'   Resampling strategy for each tasks.
#'   If only one is provided, it will be replicated to match the number of tasks.
#'   If missing, a 10-fold cross validation is used.
#' @param reg [\code{\link[batchtools]{Registry}}]\cr
#'   Registry, created by \code{\link[batchtools]{makeExperimentRegistry}}. If not explicitly passed, 
#'   uses the last created registry.
#' @return [\code{data.table}]. Generated job ids are stored in the column \dQuote{job.id}.
#' @export
#' @family benchmark
batchmark = function(learners, tasks, data.ids, resamplings, measures, models = TRUE, reg = batchtools::getDefaultRegistry()) {
  
  requirePackages("batchtools", why = "batchmark", default.method = "load")
  
  learners = ensureBenchmarkLearners(learners)
  learner.ids = extractSubList(learners, "id")
  names(learners) = learner.ids
  
  

  if (!missing(tasks)) {
    tasks = ensureBenchmarkTasks(tasks)
    task.ids = extractSubList(tasks, c("task.desc", "id"))
    names(tasks) = task.ids
  }
  if (!missing(data.ids)) {
    requirePackages("OpenML", why = "OpenML data ids given", default.method = "load")
    OpenML::populateOMLCache(data.ids = data.ids)
    did.tasks = as.list(data.ids)
    names(did.tasks) = data.ids
    tasks = if (missing(tasks))
      did.tasks
    else
      c(tasks, did.tasks)
    task.ids = names(tasks)
  } 
  if (missing(tasks))
    stop("At least one task or OpenML data ID has to be given")

  
  resamplings = ensureBenchmarkResamplings(resamplings, tasks)
  names(resamplings) = task.ids
  
  measures = ensureBenchmarkMeasures(measures, tasks)
  
  assertFlag(models)

  reg$packages = union(reg$packages, "mlr")
  
  #set name of learner list
  lrn.ids = vcapply(learners, getLearnerId)
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
  if (is.numeric(data$task))
    data$task = OpenML::convertOMLDataSetToMlr(getOMLDataSet(data$task))
  rin = makeResampleInstance(desc = data$rdesc, task = data$task)
  list(train = rin$train.inds[[i]], test = rin$test.inds[[i]], weights = rin$weights[[i]], rin = rin)
}

getAlgoFun = function(lrn, measures, models) {
  force(lrn)
  force(measures)
  force(models)
  function(job, data, instance) {

    extract.this = getExtractor(lrn)
    if (is.numeric(data$task))
      data$task = OpenML::convertOMLDataSetToMlr(getOMLDataSet(data$task))

    calculateResampleIterationResult(learner = lrn, task = data$task, train.i = instance$train, test.i = instance$test, 
      measures = measures, weights = instance$weights, rdesc = data$rdesc, model = models, extract = extract.this, show.info = FALSE)
  }
}

#' @title Reduce results of a batch-distributed benchmark.
#' 
#' @description
#' This creates a \code{\link{BenchmarkResult}} from a \code{\link[batchtools]{Registry}}.
#' To setup the benchmark have a look at \code{\link{batchmark}}.
#' 
#' @param ids [\code{data.frame} or \code{integer}]\cr
#'   A \code{data.frame} (or \code{\link[data.table]{data.table}})
#'   with a column named \dQuote{job.id}.
#'   Alternatively, you may also pass a vector of integerish job ids.
#'   If not set, defaults to all jobs.
#' @template arg_keep_pred
#' @template arg_showinfo 
#' @param reg [\code{\link[batchtools]{Registry}}]\cr
#'   Registry, created by \code{\link[batchtools]{makeExperimentRegistry}}. If not explicitly passed, 
#'   uses the last created registry.
#' @return [\code{\link{BenchmarkResult}}].
#' @export
#' @family benchmark
reduceBatchmarkResults = function(ids = NULL, keep.pred = TRUE, show.info = getMlrOption("show.info"), reg = batchtools::getDefaultRegistry()) {
  
  #registry and ids are asserted later
  requirePackages("batchtools", why = "batchmark", default.method = "load")
  assertFlag(keep.pred)
  
  result = list()
  
  problems = batchtools::getProblemIds(reg)
  algorithms = batchtools::getAlgorithmIds(reg)
  
  for (p in problems) {
    
    exps = batchtools::findExperiments(prob.name = p, ids = ids)
    job = batchtools::makeJob(id = exps[1, ])
    rin = job$instance$rin
    
    if (nrow(exps) > 0) {
      problem = job$problem
      pname = p
      if (is.numeric(problem$data$task)) {
        problem$data$task = OpenML::convertOMLDataSetToMlr(getOMLDataSet(problem$data$task))
        pname = getTaskId(problem$data$task)
      }
      
      for (a in algorithms) {
        res = batchtools::reduceResultsList(batchtools::findExperiments(prob.name = p, algo.name = a, ids = ids))
        models = !is.null(res[[1]]$model)
        lrn = problem$data$learner[[a]]
        extract.this = getExtractor(lrn)
        rs = mergeResampleResult(learner.id = a, task = problem$data$task, iter.results = res, measures = problem$data$measures, 
          rin = rin, keep.pred = keep.pred, models = models, show.info = show.info, runtime = NA, extract = extract.this)
        rs$learner = lrn
        result[[pname]][[a]] = addClasses(rs, "ResampleResult")
        }
    }
  }
  
  makeS3Obj(classes = "BenchmarkResult",
    results = result,
    measures = problem$data$measures,
    learners = problem$data$learner)
}




