#' @title Run machine learning benchmarks as distributed experiments.
#'
#' @description
#' This function is a very parallel version of [benchmark] using \pkg{batchtools}.
#' Experiments are created in the provided registry for each combination of
#' learners, tasks and resamplings. The experiments are then stored in a registry and the
#' runs can be started via [batchtools::submitJobs]. A job is one train/test split
#' of the outer resampling. In case of nested resampling (e.g. with [makeTuneWrapper]),
#' each job is a full run of inner resampling, which can be parallelized in a second step
#' with \pkg{ParallelMap}. For details on the usage and support backends have
#' a look at the batchtools tutorial page:
#' <https://github.com/mllg/batchtools>.
#'
#' The general workflow with `batchmark` looks like this:
#' \enumerate{
#' \item{Create an ExperimentRegistry using [batchtools::makeExperimentRegistry].}
#' \item{Call `batchmark(...)` which defines jobs for all learners and tasks in an [base::expand.grid] fashion.}
#' \item{Submit jobs using [batchtools::submitJobs].}
#' \item{Babysit the computation, wait for all jobs to finish using [batchtools::waitForJobs].}
#' \item{Call `reduceBatchmarkResult()` to reduce results into a [BenchmarkResult].}
#' }
#'
#' If you want to use this with \pkg{OpenML} datasets you can generate tasks from a vector
#' of dataset IDs easily with
#' `tasks = lapply(data.ids, function(x) convertOMLDataSetToMlr(getOMLDataSet(x)))`.
#' @inheritParams benchmark
#' @param resamplings [(list of) [ResampleDesc])\cr
#'   Resampling strategy for each tasks.
#'   If only one is provided, it will be replicated to match the number of tasks.
#'   If missing, a 10-fold cross validation is used.
#' @param reg ([batchtools::Registry])\cr
#'   Registry, created by [batchtools::makeExperimentRegistry]. If not explicitly passed,
#'   uses the last created registry.
#' @return ([data.table]). Generated job ids are stored in the column \dQuote{job.id}.
#' @noMd
#' @export
#' @family benchmark
batchmark = function(learners, tasks, resamplings, measures, keep.pred = TRUE,
  keep.extract = FALSE, models = FALSE, reg = batchtools::getDefaultRegistry()) {

  requirePackages("batchtools", why = "batchmark", default.method = "load")
  learners = ensureBenchmarkLearners(learners)
  tasks = ensureBenchmarkTasks(tasks)
  resamplings = ensureBenchmarkResamplings(resamplings, tasks)
  measures = ensureBenchmarkMeasures(measures, tasks)
  assertFlag(models)
  assertClass(reg, "ExperimentRegistry")

  reg$packages = union(reg$packages, "mlr")

  # generate problems
  pdes = Map(function(id, task, rin, seed) {
    batchtools::addProblem(id, data = list(rin = rin, task = task, measures = measures, learners = learners), fun = resample.fun, seed = seed, reg = reg)
    data.table(i = seq_len(rin$desc$iters))
  }, id = names(tasks), task = tasks, rin = resamplings, seed = reg$seed + seq_along(tasks))

  # generate algos
  ades = Map(function(id, learner) {
    apply.fun = getAlgoFun(learner, measures, models, keep.extract)
    batchtools::addAlgorithm(id, apply.fun, reg = reg)
    data.table()
  }, id = names(learners), learner = learners)

  # add experiments
  batchtools::addExperiments(reg = reg, prob.designs = pdes, algo.designs = ades, repls = 1L)
}

resample.fun = function(job, data, i) {
  list(train = data$rin$train.inds[[i]], test = data$rin$test.inds[[i]], weights = data$rin$weights[[i]], rdesc = data$rin$desc)
}

getAlgoFun = function(lrn, measures, models, keep.extract) {
  force(lrn)
  force(measures)
  force(models)
  if (isTRUE(keep.extract)) {
    extract.this = getExtractor(lrn)
  } else {
    extract.this = function(model) {
      NULL
    }
  }
  function(job, data, instance) {
    calculateResampleIterationResult(learner = lrn, task = data$task, train.i = instance$train, test.i = instance$test,
      measures = measures, weights = instance$weights, rdesc = instance$rdesc, model = models, extract = extract.this, show.info = FALSE)
  }
}

#' @title Reduce results of a batch-distributed benchmark.
#'
#' @description
#' This creates a [BenchmarkResult] from a [batchtools::ExperimentRegistry].
#' To setup the benchmark have a look at [batchmark].
#'
#' @param ids ([data.frame] or [integer])\cr
#'   A [base::data.frame] (or [data.table::data.table])
#'   with a column named \dQuote{job.id}.
#'   Alternatively, you may also pass a vector of integerish job ids.
#'   If not set, defaults to all successfully terminated jobs (return value of [batchtools::findDone].
#' @template arg_keep_pred
#' @template arg_keep_extract
#' @template arg_showinfo
#' @param reg ([batchtools::ExperimentRegistry])\cr
#'   Registry, created by [batchtools::makeExperimentRegistry]. If not explicitly passed,
#'   uses the last created registry.
#' @return ([BenchmarkResult]).
#' @export
#' @family benchmark
reduceBatchmarkResults = function(ids = NULL, keep.pred = TRUE, keep.extract = FALSE, show.info = getMlrOption("show.info"), reg = batchtools::getDefaultRegistry()) {

  # registry and ids are asserted later
  requirePackages("batchtools", why = "batchmark", default.method = "load")
  assertFlag(keep.pred)
  assertClass(reg, "ExperimentRegistry")

  if (is.null(ids)) {
    ids = batchtools::findDone(reg = reg)
  }
  if (NROW(ids) != nrow(batchtools::findExperiments(reg = reg))) {
    warning("Collecting results for a subset of jobs. The resulting BenchmarkResult may be misleading.")
  }

  problem = algorithm = NULL # for data.table's NSE
  tab = batchtools::getJobPars(ids, reg = reg)[, c("job.id", "problem", "algorithm")]
  setkeyv(tab, cols = c("problem", "algorithm"), physical = FALSE)
  result = namedList(tab[, unique(problem)])

  for (prob in names(result)) {
    algos = unique(tab[problem == prob], by = "algorithm")
    data = batchtools::makeJob(id = algos$job.id[1L], reg = reg)$problem$data
    result[[prob]] = namedList(algos$algorithm)

    for (algo in names(result[[prob]])) {
      res = batchtools::reduceResultsList(tab[problem == prob & algorithm == algo], reg = reg)
      models = !is.null(res[[1L]]$model)
      lrn = data$learner[[algo]]
      extract.this = getExtractor(lrn)
      rs = mergeResampleResult(learner.id = algo, task = data$task, iter.results = res, measures = data$measures,
        rin = data$rin, keep.pred = keep.pred, models = models, show.info = show.info, runtime = NA, extract = extract.this)
      rs$learner = lrn
      result[[prob]][[algo]] = addClasses(rs, "ResampleResult")
    }
  }

  makeS3Obj(classes = "BenchmarkResult",
    results = result,
    measures = data$measures,
    learners = data$learner[as.character(tab[, unique(algorithm)])])
}
