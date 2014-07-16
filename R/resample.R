#' @title Fit models according to a resampling strategy.
#'
#' @description
#'
#' \code{resample}:
#' Given a resampling strategy, which defines sets of training and test indices,
#' fits the selected learner using the training sets and performs predictions for
#' the training/test sets. This depends on what you selected in the resampling strategy,
#' see parameter \code{predict} in \code{\link{makeResampleDesc}}.
#'
#' Then performance measures are calculated on all respective data sets and aggregated.
#'
#' You are able to return all fitted models (parameter \code{models}) or extract specific parts
#' of the models (parameter \code{extract}) as returning all of them completely
#' might be memory intensive.
#'
#' For construction of the resampling strategies use the factory methods
#' \code{\link{makeResampleDesc}} and \code{\link{makeResampleInstance}}.
#'
#' The remaining functions on this page are convenience wrappers for the various
#' existing resampling strategies.
#'
#' @template arg_learner
#' @template arg_task
#' @param resampling [\code{\link{ResampleDesc}} or \code{\link{ResampleInstance}}]\cr
#'   Resampling strategy.
#'   If a description is passed, it is instantiated automatically.
#' @param iters [\code{integer(1)}]\cr
#'   See \code{\link{ResampleDesc}}.
#' @param folds [\code{integer(1)}]\cr
#'   See \code{\link{ResampleDesc}}.
#' @param reps [\code{integer(1)}]\cr
#'   See \code{\link{ResampleDesc}}.
#' @param split [\code{numeric(1)}]\cr
#'   See \code{\link{ResampleDesc}}.
#' @param stratify [\code{logical(1)}]\cr
#'   See \code{\link{ResampleDesc}}.
#' @template arg_measures
#' @param weights [\code{numeric}]\cr
#'   Optional, non-negative case weight vector to be used during fitting.
#'   If given, must be of same length as observations in task and in corresponding order.
#'   Overwrites weights specified in the \code{task}.
#'   By default \code{NULL} which means no weights are used unless specified in the task.
#' @param models [\code{logical(1)}]\cr
#'   Should all fitted models be returned?
#'   Default is \code{FALSE}.
#' @param extract [\code{function}]\cr
#'   Function used to extract information from a fitted model during resampling.
#'   Is applied to every \code{\link{WrappedModel}} resulting from calls to \code{\link{train}}
#'   during resampling.
#'   Default is to extract nothing.
#' @param ... [any]\cr
#'   Further hyperparameters passed to \code{learner}.
#' @template arg_showinfo
#' @return List of:
#'   \item{measures.test [\code{data.frame}]}{Gives you access to performance measurements
#'     on the individual test sets. Rows correspond to sets in resampling iterations,
#'     columns to performance measures.}
#'   \item{measures.train [\code{data.frame}]}{Gives you access to performance measurements
#'     on the individual training sets. Rows correspond to sets in resampling iterations,
#'     columns to performance measures. Usually not available, only if specifically requested,
#'     see general description above.}
#'   \item{aggr [\code{numeric}]}{Named vector of aggregated performance values. Names are coded like
#'     this <measure>.<aggregation>.}
#'   \item{pred [\code{\link{ResamplePrediction}}]}{Container for all predictions during resampling.}
#'   \item{models [list of \code{\link{WrappedModel}}]}{List of fitted models or \code{NULL}.}
#'   \item{extract [\code{list}]}{List of extracted parts from fitted models or \code{NULL}.}
#' @family resample
#' @export
#' @examples
#' task = makeClassifTask(data = iris, target = "Species")
#' rdesc = makeResampleDesc("CV", iters = 2)
#' r = resample(makeLearner("classif.qda"), task, rdesc)
#' print(r$aggr)
#' print(r$measures.test)
#' print(r$pred)
resample = function(learner, task, resampling, measures, weights = NULL, models = FALSE,
  extract, show.info = getMlrOption("show.info")) {

  learner = checkLearner(learner)
  assertClass(task, classes = "Task")
  # instantiate resampling
  if (inherits(resampling, "ResampleDesc"))
    resampling = makeResampleInstance(resampling, task = task)
  assertClass(resampling, classes = "ResampleInstance")
  measures = checkMeasures(measures, task)
  if (!is.null(weights)) {
    assertNumeric(weights, len = task$task.desc$size, any.missing = FALSE, lower = 0)
  }
  assertFlag(models)
  if (missing(extract))
    extract = function(model) {}
  else
    assertFunction(extract)
  assertFlag(show.info)

  n = task$task.desc$size
  r = resampling$size
  if (n != r)
    stop(paste("Size of data set:", n, "and resampling instance:", r, "differ!"))

  checkTaskCreationLearner(task, learner, weights)

  rin = resampling
  more.args = list(learner = learner, task = task, rin = rin,
    measures = measures, model = models, extract = extract, show.info = show.info)
  if (!is.null(weights))
    weights = task$weights
  more.args = setValue(more.args, "weights", weights)
  parallelLibrary("mlr", master = FALSE, level = "mlr.resample", show.info = FALSE)
  exportMlrOptions(level = "mlr.resample")
  iter.results = parallelMap(doResampleIteration, seq_len(rin$desc$iters), level = "mlr.resample", more.args = more.args)
  mergeResampleResult(task, iter.results, measures, rin, models, extract, show.info)
}

doResampleIteration = function(learner, task, rin, i, measures, weights, model, extract, show.info) {
  setSlaveOptions()
  if (show.info)
    messagef("[Resample] %s iter: %i", rin$desc$id, i)
  train.i = rin$train.inds[[i]]
  test.i = rin$test.inds[[i]]

  m = train(learner, task, subset = train.i, weights = weights[train.i])

  # does a measure require to calculate pred.train?
  ms.train = rep(NA, length(measures))
  ms.test = rep(NA, length(measures))
  pred.train = NULL
  pred.test = NULL
  pp = rin$desc$predict
  if (pp == "train") {
    pred.train = predict(m, task, subset = train.i)
    ms.train = sapply(measures, function(pm) performance(task = task, model = m, pred = pred.train, measures = pm))
  } else if (pp == "test") {
    pred.test = predict(m, task, subset = test.i)
    ms.test = sapply(measures, function(pm) performance(task = task, model = m, pred = pred.test, measures = pm))
  } else { # "both"
    pred.train = predict(m, task, subset = train.i)
    ms.train = sapply(measures, function(pm) performance(task = task, model = m, pred = pred.train, measures = pm))
    pred.test = predict(m, task, subset = test.i)
    ms.test = sapply(measures, function(pm) performance(task = task, model = m, pred = pred.test, measures = pm))
  }
  ex = extract(m)
  list(
    measures.test = ms.test,
    measures.train = ms.train,
    model = if (model) m else NULL,
    pred.test = pred.test,
    pred.train = pred.train,
    extract = ex
  )
}

mergeResampleResult = function(task, iter.results, measures, rin, models, extract, show.info) {
  iters = length(iter.results)
  mids = sapply(measures, function(m) m$id)

  ms.test = extractSubList(iter.results, "measures.test", simplify = FALSE)
  ms.test = as.data.frame(do.call(rbind, ms.test))
  colnames(ms.test) = sapply(measures, function(pm) pm$id)
  rownames(ms.test) = NULL
  ms.test = cbind(iter = seq_len(iters), ms.test)

  ms.train = extractSubList(iter.results, "measures.train", simplify = FALSE)
  ms.train = as.data.frame(do.call(rbind, ms.train))
  colnames(ms.train) = mids
  rownames(ms.train) = NULL
  ms.train = cbind(iter = 1:iters, ms.train)

  preds.test = extractSubList(iter.results, "pred.test", simplify = FALSE)
  preds.train = extractSubList(iter.results, "pred.train", simplify = FALSE)
  pred = makeResamplePrediction(instance = rin, preds.test = preds.test, preds.train = preds.train)

  aggr = sapply(measures, function(m)  m$aggr$fun(task, ms.test[, m$id], ms.train[, m$id], m, rin$group, pred))
  names(aggr) = sapply(measures, measureAggrName)
  if (show.info) {
    messagef("[Resample] Result: %s", perfsToString(aggr))
  }
  list(
    measures.train = ms.train,
    measures.test = ms.test,
    aggr = aggr,
    pred = pred,
    models = if(models) lapply(iter.results, function(x) x$model) else NULL,
    extract = if(is.function(extract)) extractSubList(iter.results, "extract", simplify = FALSE) else NULL
  )
}

