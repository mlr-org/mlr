#' @title Benchmark experiment for multiple learners and tasks.
#'
#' @description
#' Complete benchmark experiment to compare different learning algorithms across one or more tasks
#' w.r.t. a given resampling strategy for each task. Experiments are paired, meaning always the same
#' training / test sets are used for the different learners.
#'
#' You can also get automatic, internal tuning by using \code{\link{makeTuneWrapper}} with your learner.
#'
#' @param learners [\code{\link{Learner}} | list of them]\cr
#'   Learning algorithms which should be compared.
#' @param tasks [\code{\link{SupervisedTask}} | list of them]\cr
#'   Tasks that learners should be run on.
#' @param resamplings [\code{\link{ResampleDesc}} | \code{\link{ResampleInstance}} | list of them]\cr
#'   Resampling strategy for each tasks. Provided one it is used for every task otherwise each resampling
#'   strategy is bounded to one task.
#' @param measures [\code{\link{Measure}} | list of them]\cr
#'   Performance measures.
#' @return [\code{BenchmarkResult}].
#' @export
benchmark = function(learners, tasks, resamplings, measures) {

  # check learners
  if (inherits(learners, "Learner"))
    learners = list(learners)
  checkArg(learners, "list")
  checkListElementClass(learners, "Learner")
  if (!length(learners))
    stop("No learners were passed!")
  learner.ids = extractSubList(learners, "id")
  if (any(duplicated(learner.ids)))
    stop("Learners need unique ids!")
  names(learners) = learner.ids

  # check tasks
  if (inherits(tasks, "SupervisedTask"))
    tasks = list(tasks)
  checkArg(tasks, "list")
  checkListElementClass(tasks, "SupervisedTask")
  if (!length(tasks))
    stop("No tasks were passed!")

  task.ids = extractSubList(tasks,"task.desc")["id",]
  if (any(duplicated(task.ids)))
    stop("Tasks need unique ids!")
  names(tasks) = task.ids

  # check resamplings
  if (missing(resamplings))
     resamplings = makeResampleDesc("CV", iters = 10L)
  if (inherits(resamplings, "ResampleInstance") || inherits(resamplings, "ResampleDesc"))
    resamplings = replicate(length(tasks), resamplings, simplify = FALSE)
  if (length(resamplings) != length(tasks))
    stop("Number of resampling strategies and number of tasks differ!")
  names(resamplings) = task.ids

  # check measures
  if (missing(measures)) {
    measures = mlr:::default.measures(tasks[[1]])
  } else {
    if (inherits(measures, "Measure"))
     measures = list(measures)
  }

  measure.ids = extractSubList(measures, "id")

  # instantiate resampling
  resamplings = Map(function(res, tt) {
    if (is(res, "ResampleInstance"))
      res
    else
      makeResampleInstance(res,task = tt)
  }, resamplings, tasks)

  inds.mat = expand.grid(task = task.ids, learner = learner.ids, stringsAsFactors=FALSE)
  inds = split(as.matrix(inds.mat), f = seq_row(inds.mat))
  more.args = list(learners = learners, tasks = tasks, resamplings = resamplings, measures = measures)
  results = parallelMap(benchmarkParallel, inds, more.args = more.args)
  results.by.task = split(results, unlist(inds.mat$task))
  for(taskname in names(results.by.task)){
    names(results.by.task[[taskname]]) = unlist(inds.mat$learner)[unlist(inds.mat$task) == taskname]
  }
  results.by.task = addClasses(results.by.task, "BenchmarkResult")
  results.by.task
}

benchmarkParallel = function(index, learners, tasks, resamplings, measures) {
  ind.learner = index[2L][[1L]]
  ind.task = index[1L][[1L]]
  messagef("Task: %s, Learner: %s", ind.task, ind.learner)
  if("FeatSelWrapper" %in% class(learners[[ind.learner]]))
    extract.this = getFeatSelResult
  else if("TuneWrapper" %in% class(learners[[ind.learner]]))
    extract.this = getTuneResult
  #else if("FilterWrapper" %in% class(learners[[ind.learner]]))
  #  extract.this = getFilteredFeatures
  else
    extract.this = function(model) {}
  resample(learners[[ind.learner]], tasks[[ind.task]], resamplings[[ind.task]], measures = measures, model = TRUE, extract = extract.this)
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

#' @S3method getAggrMeasures BenchmarkResult
getAggrMeasures.BenchmarkResult = function(results) {
  res = lapply(names(results), function(task.name) {
    lapply(names(results[[task.name]]), function(learner.name) {
      res = data.frame(task=task.name, learner=learner.name)
      cbind(res, t(results[[task.name]][[learner.name]]$aggr))
    })
  })
  do.call(rbind, unlist(res, recursive=FALSE))
}

#' @S3method print BenchmarkResult
print.BenchmarkResult = function(results) {
  print(getAggrMeasures.BenchmarkResult(results))
}

#' @S3method as.data.frame BenchmarkResult
as.data.frame.BenchmarkResult = function(results) {
  getAggrMeasures.BenchmarkResult(results)
}

#' Extract the prediction inforamtions of an object
#'
#' @param object [\code{\link{BenchmarkResult}}]\cr
#'   Object which contains the predictions
#' @return [\code{data.frame}].
#' @export
#' @aliases getPredictions
getPredictions = function(object) {
  UseMethod("makeWrappedModel")
}

#' @S3method getPredictions BenchmarkResult
getPredictions.BenchmarkResult = function(results) {
  res = lapply(names(results), function(task.name) {
    t.res = lapply(names(results[[task.name]]), function(learner.name) {
      l.res = data.frame(results[[task.name]][[learner.name]]$pred)[, "response", drop = FALSE]
      names(l.res) = paste0("response.", learner.name)
      l.res
    })
    t.res.df = as.data.frame(results[[task.name]][[1]]$pred)[,c("id", "truth", "iter", "set")]
    t.res.df = cbind(t.res.df, do.call(cbind, t.res))
    t.res.df
  })
  names(res) = names(results)
  res
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

#' @S3method getMeasures BenchmarkResult
getMeasures.BenchmarkResult = function(results) {
  res = lapply(names(results), function(task.name) {
    t.res = lapply(names(results[[task.name]]), function(learner.name) {
      l.res = results[[task.name]][[learner.name]]$measures.test
      l.res = subset(l.res, select=-iter)
      names(l.res) = paste0(learner.name, ".", names(l.res))
      l.res
    })
    t.res.df = results[[task.name]][[1]]$measures.test[, c("iter"), drop = FALSE]
    t.res.df = cbind(t.res.df, do.call(cbind, t.res))
    t.res.df
  })
  names(res) = names(results)
  res
}

# #' @S3method getExtract BenchmarkResult
getExtract.BenchmarkResult = function(results, what, within="extract"){
  if(missing(what))
    what = NULL
  res = lapply(results, function(task) {
    t.res = lapply(task, function(learner) {
      if (is.null(what) || what %in% class(learner[[within]][[1]]))
        learner[[within]]
      else
        NULL
    })
  })
  res
}

#' @S3method getFeatSelResult BenchmarkResult
getFeatSelResult.BenchmarkResult = function(results) {
  getExtract.BenchmarkResult(results, "FeatSelResult")
}

#' @S3method getTuneResult BenchmarkResult
getTuneResult.BenchmarkResult = function(results) {
  getExtract.BenchmarkResult(results, "TuneResult")
}


