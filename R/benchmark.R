#' Benchmark experiment for multiple learners and tasks.
#'
#' Complete benchmark experiment to compare different learning algorithms across one or more tasks
#' w.r.t. a given resampling strategy. Experiments are paired, meaning always the same
#' training / test sets are used for the different learners.
#' Furhtermore, your learners can be automatically tuned using \code{\link{makeTuneWrapper}}.
#'
#' @param learners [(list of) \code{\link{Learner}}]\cr
#'   Learning algorithms which should be compared.
#' @param tasks [(list of) \code{\link{SupervisedTask}}]\cr
#'   Tasks that learners should be run on.
#' @param resamplings [(list of) \code{\link{ResampleDesc}} | \code{\link{ResampleInstance}}]\cr
#'   Resampling strategy for each tasks.
#'   If only one is provided, it will be replicated to match the number of tasks.
#'   If missing, a 10-fold cross validation is used.
#' @param measures [(list of) \code{\link{Measure}}]\cr
#'   Performance measures.
#'   If missing, the default measure of the first task is used.
#' @return [\code{BenchmarkResult}].
#' @export
benchmark = function(learners, tasks, resamplings, measures) {
  # check learners
  if (inherits(learners, "Learner")) {
    learners = list(learners)
  } else {
    checkArg(learners, "list")
    checkListElementClass(learners, "Learner")
    if (!length(learners))
      stop("No learners were passed!")
  }
  learner.ids = extractSubList(learners, "id")
  if (anyDuplicated(learner.ids))
    stop("Learners need unique ids!")
  names(learners) = learner.ids

  # check tasks
  if (inherits(tasks, "SupervisedTask")) {
    tasks = list(tasks)
  } else {
    checkArg(tasks, "list")
    checkListElementClass(tasks, "SupervisedTask")
    if (!length(tasks))
      stop("No tasks were passed!")
  }
  task.ids = extractSubList(tasks, "task.desc")["id",]
  if (anyDuplicated(task.ids))
    stop("Tasks need unique ids!")
  names(tasks) = task.ids

  # check resamplings
  if (missing(resamplings)) {
     resamplings = replicate(length(tasks), makeResampleDesc("CV", iters = 10L), simplify = FALSE)
  } else if (inherits(resamplings, "ResampleInstance") || inherits(resamplings, "ResampleDesc")) {
    resamplings = replicate(length(tasks), resamplings, simplify = FALSE)
  } else {
    checkArg(resamplings, "list")
    if (length(resamplings) != length(tasks))
      stop("Number of resampling strategies and number of tasks differ!")
  }
  resamplings = Map(function(res, tt) {
    if (inherits(res, "ResampleInstance"))
      return(res)
    if (inherits(res, "ResampleDesc"))
      return(makeResampleInstance(res, task = tt))
    stop("All objects in 'resamplings' must be of class 'ResampleDesc' or 'ResampleInstance'")
  }, resamplings, tasks)
  names(resamplings) = task.ids

  # check measures
  if (missing(measures)) {
    measures = default.measures(tasks[[1L]])
  } else if (inherits(measures, "Measure")) {
      measures = list(measures)
  } else {
    checkArg(measures, "list")
    checkListElementClass(measures, "Measure")
  }

  inds = expand.grid(task = task.ids, learner = learner.ids, stringsAsFactors = FALSE)

  plevel = "mlr.benchmark"
  parallelLibrary("mlr", master = FALSE, level = plevel, show.info = FALSE)
  exportMlrOptions()
  results = parallelMap(
    benchmarkParallel,
    split(as.matrix(inds), f = seq_row(inds)),
    more.args = list(learners = learners, tasks = tasks, resamplings = resamplings, measures = measures),
    level = plevel
  )
  results.by.task = split(results, unlist(inds$task))
  for(taskname in names(results.by.task)) {
    names(results.by.task[[taskname]]) = inds$learner[inds$task == taskname]
  }
  addClasses(results.by.task, "BenchmarkResult")
}

#' Result of a benchmark
#'
#' Container for results of benchmarked experiments using \code{\link{benchmark}}.
#' The structure of the object itself is rather complicated, it is recommended to
#' retrive required information via \code{link{getAggrMeasures}}, \code{link{getPredictions}},
#' \code{\link{getMeasures}}, \code{\link{getFeatSelResult}}, \code{\link{getTuneResult}} or
#' \code{\link{getFilterResult}}. Alternatively, you can convert the object using
#' \code{\link[base]{as.data.frame}}
#'
#' @name BenchmarkResult
#' @rdname BenchmarkResult
NULL

benchmarkParallel = function(index, learners, tasks, resamplings, measures) {
  ind.task = index[1L][[1L]]
  ind.learner = index[2L][[1L]]
  messagef("Task: %s, Learner: %s", ind.task, ind.learner)
  if("FeatSelWrapper" %in% class(learners[[ind.learner]])) {
    extract.this = getFeatSelResult
  } else if("TuneWrapper" %in% class(learners[[ind.learner]])) {
    extract.this = getTuneResult
  } else if("FilterWrapper" %in% class(learners[[ind.learner]])) {
    extract.this = getFilterResult
  } else {
    extract.this = function(model) { NULL }
  }
  resample(learners[[ind.learner]], tasks[[ind.task]], resamplings[[ind.task]], measures = measures, models = TRUE, extract = extract.this)
}

#' Extract the aggregated measures of an object.
#'
#' @param object [\code{\link{BenchmarkResult}}]\cr
#'   Object which contains the aggregated measures.
#' @return [\code{data.frame}].
#' @export
#' @aliases getAggrMeasures
getAggrMeasures = function(object) {
  UseMethod("getAggrMeasures")
}

#' @export
getAggrMeasures.BenchmarkResult = function(object) {
  task.names = names(object)
  learner.names = unname(lapply(object, names))
  df = data.frame(
    task = rep.int(task.names, viapply(learner.names, length)),
    learner = unlist(learner.names)
  )
  aggr = rowLapply(df, function(x) t(object[[x$task]][[x$learner]]$aggr))
  cbind(df, do.call(rbind, aggr))
}

#' @export
print.BenchmarkResult = function(x, ...) {
  print(getAggrMeasures.BenchmarkResult(x))
}

#' @export
as.data.frame.BenchmarkResult = function(x, ...) {
  getAggrMeasures.BenchmarkResult(x)
}

#' Extract the prediction inforamation of an object
#'
#' @param object [\code{\link{BenchmarkResult}}]\cr
#'   Object which contains the predictions
#' @return [\code{data.frame}].
#' @export
#' @aliases getPredictions
getPredictions = function(object) {
  UseMethod("getPredictions")
}

#' @export
getPredictions.BenchmarkResult = function(object) {
  extractResponse = function(learner.name, task.name) {
    setNames(data.frame(object[[task.name]][[learner.name]]$pred)[, "response", drop = FALSE],
      paste0("response.", learner.name))
  }
  setNames(lapply(names(object), function(task.name) {
    cbind(
      as.data.frame(object[[task.name]][[1L]]$pred)[, c("id", "truth", "iter", "set"), drop = FALSE],
      do.call(cbind, lapply(names(object[[task.name]]), extractResponse, task.name = task.name))
    )
  }), names(object))
}

#' Extract the measure results of an object
#'
#' @param object [\code{\link{BenchmarkResult}}]\cr
#'   Object which contains the measure results.
#' @return [\code{data.frame}].
#' @export
#' @aliases getMeasures
getMeasures = function(object) {
  UseMethod("getMeasures")
}

#' @export
getMeasures.BenchmarkResult = function(object) {
  extractMeasures = function(learner.name, task.name) {
      x = dropNamed(object[[task.name]][[learner.name]]$measures.test, "iter")
      setNames(x, paste0(learner.name, ".", names(x)))
  }
  setNames(lapply(names(object), function(task.name) {
    cbind(
      object[[task.name]][[1L]]$measures.test[, "iter", drop = FALSE],
      do.call(cbind, lapply(names(object[[task.name]]), extractMeasures, task.name = task.name))
    )
  }), names(object))
}

getExtract = function(object, what, within = "extract") {
  if(missing(what))
    what = NULL
  lapply(object, function(task) {
    t.res = lapply(task, function(learner) {
      if (is.null(what) || what %in% class(learner[[within]][[1L]]))
        learner[[within]]
      else
        NULL
    })
  })
}

#' @export
getFeatSelResult.BenchmarkResult = function(object) {
  getExtract(object, "FeatSelResult")
}

#' @export
getTuneResult.BenchmarkResult = function(object) {
  getExtract(object, "TuneResult")
}

#' @export
getFilterResult.BenchmarkResult = function(object) {
  getExtract(object, "FilterResult")
}
