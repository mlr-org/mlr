#' @title Fit models according to a resampling strategy.
#'
#' @description
#' The function \code{resample} fits a model specified by \link{Learner} on a \link{Task}
#' and calculates predictions and performance \link{measures} for all training
#' and all test sets specified by a either a resampling description (\link{ResampleDesc})
#' or resampling instance (\link{ResampleInstance}).
#'
#' You are able to return all fitted models (parameter \code{models}) or extract specific parts
#' of the models (parameter \code{extract}) as returning all of them completely
#' might be memory intensive.
#'
#' The remaining functions on this page are convenience wrappers for the various
#' existing resampling strategies. Note that if you need to work with precomputed training and
#' test splits (i.e., resampling instances), you have to stick with \code{resample}.
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
#' @template arg_keep_pred
#' @param ... [any]\cr
#'   Further hyperparameters passed to \code{learner}.
#' @template arg_showinfo
#' @return [\code{\link{ResampleResult}}].
#' @family resample
#' @note If you would like to include results from the training data set, make
#' sure to appropriately adjust the resampling strategy and the aggregation for
#' the measure. See example code below.
#' @export
#' @examples
#' task = makeClassifTask(data = iris, target = "Species")
#' rdesc = makeResampleDesc("CV", iters = 2)
#' r = resample(makeLearner("classif.qda"), task, rdesc)
#' print(r$aggr)
#' print(r$measures.test)
#' print(r$pred)
#'
#' # include the training set performance as well
#' rdesc = makeResampleDesc("CV", iters = 2, predict = "both")
#' r = resample(makeLearner("classif.qda"), task, rdesc,
#'   measures = list(mmce, setAggregation(mmce, train.mean)))
#' print(r$aggr)
resample = function(learner, task, resampling, measures, weights = NULL, models = FALSE,
  extract, keep.pred = TRUE, ..., show.info = getMlrOption("show.info")) {

  learner = checkLearner(learner)
  learner = setHyperPars(learner, ...)
  assertClass(task, classes = "Task")
  n = getTaskSize(task)
  # instantiate resampling
  if (inherits(resampling, "ResampleDesc"))
    resampling = makeResampleInstance(resampling, task = task)
  assertClass(resampling, classes = "ResampleInstance")
  measures = checkMeasures(measures, task)
  if (!is.null(weights)) {
    assertNumeric(weights, len = n, any.missing = FALSE, lower = 0)
  }
  assertFlag(models)
  if (missing(extract))
    extract = function(model) {}
  else
    assertFunction(extract)
  assertFlag(show.info)

  r = resampling$size
  if (n != r)
    stop(stri_paste("Size of data set:", n, "and resampling instance:", r, "differ!", sep = " "))

  checkLearnerBeforeTrain(task, learner, weights)
  checkAggrsBeforeResample(measures, resampling$desc)

  rin = resampling
  more.args = list(learner = learner, task = task, rin = rin, weights = NULL,
    measures = measures, model = models, extract = extract, show.info = show.info)
  if (!is.null(weights)) {
    more.args$weights = weights
  } else if (!is.null(getTaskWeights(task))) {
    more.args$weights = getTaskWeights(task)
  }
  parallelLibrary("mlr", master = FALSE, level = "mlr.resample", show.info = FALSE)
  exportMlrOptions(level = "mlr.resample")
  time1 = Sys.time()
  iter.results = parallelMap(doResampleIteration, seq_len(rin$desc$iters), level = "mlr.resample", more.args = more.args)
  time2 = Sys.time()
  runtime = as.numeric(difftime(time2, time1, "sec"))
  addClasses(
    mergeResampleResult(learner, task, iter.results, measures, rin, models, extract, keep.pred, show.info, runtime),
    "ResampleResult"
  )
}

doResampleIteration = function(learner, task, rin, i, measures, weights, model, extract, show.info) {
  setSlaveOptions()
  if (show.info)
    messagef("[Resample] Start %s iter: %i", rin$desc$id, i)
  train.i = rin$train.inds[[i]]
  test.i = rin$test.inds[[i]]

  err.msgs = c(NA_character_, NA_character_)
  m = train(learner, task, subset = train.i, weights = weights[train.i])
  if (isFailureModel(m))
    err.msgs[1L] = getFailureModelMsg(m)

  # does a measure require to calculate pred.train?
  ms.train = rep(NA, length(measures))
  ms.test = rep(NA, length(measures))
  pred.train = NULL
  pred.test = NULL
  pp = rin$desc$predict
  if (pp == "train") {
    pred.train = predict(m, task, subset = train.i)
    if (!is.na(pred.train$error)) err.msgs[2L] = pred.train$error
    ms.train = vnapply(measures, function(pm) performance(task = task, model = m, pred = pred.train, measures = pm))
  } else if (pp == "test") {
    pred.test = predict(m, task, subset = test.i)
    if (!is.na(pred.test$error)) err.msgs[2L] = pred.test$error
    ms.test = vnapply(measures, function(pm) performance(task = task, model = m, pred = pred.test, measures = pm))
  } else { # "both"
    pred.train = predict(m, task, subset = train.i)
    if (!is.na(pred.train$error)) err.msgs[2L] = pred.train$error
    ms.train = vnapply(measures, function(pm) performance(task = task, model = m, pred = pred.train, measures = pm))
    pred.test = predict(m, task, subset = test.i)
    if (!is.na(pred.test$error)) err.msgs[2L] = paste(err.msgs[2L], pred.test$error)
    ms.test = vnapply(measures, function(pm) performance(task = task, model = m, pred = pred.test, measures = pm))
  }
  ex = extract(m)
  if (show.info) {
    mids = vcapply(measures, function(m) m$id)
    for (i in 1:length(mids)) {
      messagef("[Resample] Result: %s = %f", mids[i] , ms.test[i])
    }
  }
  list(
    measures.test = ms.test,
    measures.train = ms.train,
    model = if (model) m else NULL,
    pred.test = pred.test,
    pred.train = pred.train,
    err.msgs = err.msgs,
    extract = ex
  )
}

mergeResampleResult = function(learner, task, iter.results, measures, rin, models, extract, keep.pred, show.info, runtime) {
  iters = length(iter.results)
  mids = vcapply(measures, function(m) m$id)

  ms.train = as.data.frame(extractSubList(iter.results, "measures.train", simplify = "rows"))

  ms.test = extractSubList(iter.results, "measures.test", simplify = FALSE)
  ms.test = as.data.frame(do.call(rbind, ms.test))

  preds.test = extractSubList(iter.results, "pred.test", simplify = FALSE)
  preds.train = extractSubList(iter.results, "pred.train", simplify = FALSE)
  pred = makeResamplePrediction(instance = rin, preds.test = preds.test, preds.train = preds.train)

  # aggr = vnapply(measures, function(m) m$aggr$fun(task, ms.test[, m$id], ms.train[, m$id], m, rin$group, pred))
  aggr = vnapply(seq_along(measures), function(i) {
    m = measures[[i]]
    m$aggr$fun(task, ms.test[, i], ms.train[, i], m, rin$group, pred)
  })
  names(aggr) = vcapply(measures, measureAggrName)

  # name ms.* rows and cols
  colnames(ms.test) = mids
  rownames(ms.test) = NULL
  ms.test = cbind(iter = seq_len(iters), ms.test)
  colnames(ms.train) = mids
  rownames(ms.train) = NULL
  ms.train = cbind(iter = seq_len(iters), ms.train)

  err.msgs = as.data.frame(extractSubList(iter.results, "err.msgs", simplify = "rows"))
  rownames(err.msgs) = NULL
  colnames(err.msgs) = c("train", "predict")
  err.msgs = cbind(iter = seq_len(iters), err.msgs)

  if (show.info)
    messagef("[Resample] Result: %s", perfsToString(aggr))

  if (!keep.pred)
    pred = NULL

  list(
    learner.id = learner$id,
    task.id = getTaskId(task),
    measures.train = ms.train,
    measures.test = ms.test,
    aggr = aggr,
    pred = pred,
    models = if (models) lapply(iter.results, function(x) x$model) else NULL,
    err.msgs = err.msgs,
    extract = if (is.function(extract)) extractSubList(iter.results, "extract", simplify = FALSE) else NULL,
    runtime = runtime
  )
}
