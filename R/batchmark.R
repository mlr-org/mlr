#' @title Run machine learning benchmarks as distributed experiments.
#'
#' @description
#' This function is a very parallel version of \code{\link{benchmark}} using \pkg{batchtools}.
#' Experiments are created in the provided registry for each combination of
#' learners, tasks and resamplings. The experiments are then stored in a registry and the
#' runs can be started via \code{\link[batchtools]{submitJobs}}. A job is one train/test split
#' of the outer resampling. In case of nested resampling (e.g. with \code{\link{makeTuneWrapper}}),
#' each job is a full run of inner resampling, which can be parallelized in a second step
#' with \pkg{ParallelMap}. For details on the usage and support backends have
#' a look at the batchtools tutorial page:
#' \url{https://github.com/mllg/batchtools}.
#'
#' The general workflow with \code{batchmark} looks like this:
#' \enumerate{
#' \item{Create an ExperimentRegistry using \code{\link[batchtools]{makeExperimentRegistry}}.}
#' \item{Call \code{batchmark(...)} which defines jobs for all learners and tasks in an \code{\link[base]{expand.grid}} fashion.}
#' \item{Submit jobs using \code{\link[batchtools]{submitJobs}}.}
#' \item{Babysit the computation, wait for all jobs to finish using \code{\link[batchtools]{waitForJobs}}.}
#' \item{Call \code{reduceBatchmarkResult()} to reduce results into a \code{\link{BenchmarkResult}}.}
#' }
#'
#' If you want to use this with \pkg{OpenML} datasets you can generate tasks from a vector
#' of dataset IDs easily with
#' \code{tasks = lapply(data.ids, function(x) convertOMLDataSetToMlr(getOMLDataSet(x)))}.
#' @inheritParams benchmark
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
batchmark = function(learners, tasks, resamplings, measures, models = TRUE, reg = batchtools::getDefaultRegistry()) {
  requirePackages("batchtools", why = "batchmark", default.method = "load")
  assertFlag(models)

  learners = ensureBenchmarkLearners(learners)
  learner.ids = vcapply(learners, getLearnerId)
  names(learners) = learner.ids

  tasks = ensureBenchmarkTasks(tasks)
  task.ids = vcapply(tasks, getTaskId)
  names(tasks) = task.ids

  resamplings = ensureBenchmarkResamplings(resamplings, tasks)
  names(resamplings) = task.ids

  measures = ensureBenchmarkMeasures(measures, tasks)

  reg$packages = union(reg$packages, "mlr")

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
  list(train = rin$train.inds[[i]], test = rin$test.inds[[i]], weights = rin$weights[[i]], rin = rin)
}

getAlgoFun = function(lrn, measures, models) {
  force(lrn)
  force(measures)
  force(models)
  function(job, data, instance) {
    extract.this = getExtractor(lrn)
    calculateResampleIterationResult(learner = lrn, task = data$task, train.i = instance$train, test.i = instance$test,
      measures = measures, weights = instance$weights, rdesc = data$rdesc, model = models, extract = extract.this, show.info = FALSE)
  }
}

#' @title Reduce results of a batch-distributed benchmark.
#'
#' @description
#' This creates a \code{\link{BenchmarkResult}} from a \code{\link[batchtools]{ExperimentRegistry}}.
#' To setup the benchmark have a look at \code{\link{batchmark}}.
#'
#' @param ids [\code{data.frame} or \code{integer}]\cr
#'   A \code{\link[base]{data.frame}} (or \code{\link[data.table]{data.table}})
#'   with a column named \dQuote{job.id}.
#'   Alternatively, you may also pass a vector of integerish job ids.
#'   If not set, defaults to all jobs.
#' @template arg_keep_pred
#' @template arg_showinfo
#' @param reg [\code{\link[batchtools]{ExperimentRegistry}}]\cr
#'   Registry, created by \code{\link[batchtools]{makeExperimentRegistry}}. If not explicitly passed,
#'   uses the last created registry.
#' @return [\code{\link{BenchmarkResult}}].
#' @export
#' @family benchmark
reduceBatchmarkResults = function(ids = NULL, keep.pred = TRUE, show.info = getMlrOption("show.info"), reg = batchtools::getDefaultRegistry()) {
  # registry and ids are asserted later
  requirePackages("batchtools", why = "batchmark", default.method = "load")
  assertFlag(keep.pred)

  problems = batchtools::getProblemIds(reg)
  algorithms = batchtools::getAlgorithmIds(reg)

  result = replicate(length(problems), namedList(algorithms), simplify = FALSE)
  names(result) = problems

  for (prob in problems) {
    exps = batchtools::findExperiments(prob.name = prob, ids = ids)
    job = batchtools::makeJob(id = exps[1, ])
    rin = job$instance$rin

    if (nrow(exps) > 0) {
      problem = job$problem

      for (a in algorithms) {
        # FIXME: Use prob.name + algo.name as soon as batchtools-0.9.3 hits CRAN
        res = batchtools::reduceResultsList(batchtools::findExperiments(prob.pattern = stri_paste("^", prob, "$"),
          algo.pattern = stri_paste("^", a, "$"), ids = ids))
        models = !is.null(res[[1L]]$model)
        lrn = problem$data$learner[[a]]
        extract.this = getExtractor(lrn)
        rs = mergeResampleResult(learner.id = a, task = problem$data$task, iter.results = res, measures = problem$data$measures,
          rin = rin, keep.pred = keep.pred, models = models, show.info = show.info, runtime = NA, extract = extract.this)
        rs$learner = lrn
        result[[prob]][[a]] = addClasses(rs, "ResampleResult")
        }
    }
  }

  makeS3Obj(classes = "BenchmarkResult",
    results = result,
    measures = problem$data$measures,
    learners = problem$data$learner)
}
