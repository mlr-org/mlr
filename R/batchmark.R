#' @title Run Machine Learning Benchmarks as Experiments
#' @description
#' This function is a very parallel version of \code{\link{benchmark}}.
#' Experiments are created in the provided registry for each combination of
#' learners, tasks and resamplings. The experiments are then stored in a registry and the 
#' runs can be started via \code{\link[batchtools]{submitJobs}]}. For details on the usage
#' and support backends have a look at the batchtools tutorial page: 
#' \url{https://github.com/mllg/batchtools}.
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
batchmark = function(learners, tasks, resamplings, measures = NULL, models = TRUE, reg = getDefaultRegistry()) {
  
  requirePackages("batchtools", why = "batchmark", default.method = "attach")
  batchtools:::assertRegistry(reg)
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

  # generate problems
  pdes = Map(function(id, task, rdesc, seed) {
    addProblem(id, data = list(rdesc = rdesc, task = task, measures = measures, learners = learners), fun = resample.fun, seed = seed, reg = reg)
    data.table(i = seq_len(rdesc$iters))
  }, id = task.ids, task = tasks, rdesc = resamplings, seed = reg$seed + seq_along(tasks))

  # generate algos
  ades = Map(function(id, learner) {
    apply.fun = getAlgoFun(learner, measures, models)
    addAlgorithm(id, apply.fun, reg = reg)
    data.table()
  }, id = learner.ids, learner = learners)

  # add experiments
  addExperiments(reg = reg, prob.designs = pdes, algo.designs = ades, repls = 1L)
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


reduceBatchmarkResults = function(ids = NULL, keep.pred = TRUE, reg = getDefaultRegistry()) {
  
  batchtools:::assertRegistry(reg)
  assertFlag(keep.pred)
  
  problems = getProblemIds(reg)
  algorithms = getAlgorithmIds(reg)
  
  result = lapply(seq_along(problems), function(p) {
    problem = readRDS(paste0(reg$file.dir, "/problems/", problems[p], ".rds"))
    rin = makeResampleInstance(problem$data$rdesc, problem$data$task)
    rr = lapply(seq_along(algorithms), function(a) {
      res = reduceResultsList(findExperiments(prob.name = problems[p], algo.name = algorithms[a], ids = ids))
      models = !is.null(res[[1]]$model)
      lrn = problem$data$learner[[a]]
      extract.this = getExtractor(lrn)
      mergeResampleResult(learner.id = problems[a], task = problem$data$task, iter.results = res, measures = problem$data$measures, 
        rin = rin, keep.pred = keep.pred, models = models, show.info = FALSE, runtime = NA, extract = extract.this)
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




