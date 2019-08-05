#' @title Fit models according to a resampling strategy.
#'
#' @description
#' The function `resample` fits a model specified by \link{Learner} on a \link{Task}
#' and calculates predictions and performance \link{measures} for all training
#' and all test sets specified by a either a resampling description (\link{ResampleDesc})
#' or resampling instance (\link{ResampleInstance}).
#'
#' You are able to return all fitted models (parameter `models`) or extract specific parts
#' of the models (parameter `extract`) as returning all of them completely
#' might be memory intensive.
#'
#' The remaining functions on this page are convenience wrappers for the various
#' existing resampling strategies. Note that if you need to work with precomputed training and
#' test splits (i.e., resampling instances), you have to stick with `resample`.
#'
#' @template arg_learner
#' @template arg_task
#' @param resampling ([ResampleDesc] or [ResampleInstance])\cr
#'   Resampling strategy.
#'   If a description is passed, it is instantiated automatically.
#' @param iters (`integer(1)`)\cr
#'   See [ResampleDesc].
#' @param folds (`integer(1)`)\cr
#'   See [ResampleDesc].
#' @param reps (`integer(1)`)\cr
#'   See [ResampleDesc].
#' @param split (`numeric(1)`)\cr
#'   See [ResampleDesc].
#' @param stratify (`logical(1)`)\cr
#'   See [ResampleDesc].
#' @param horizon (`numeric(1)`)\cr
#'   See [ResampleDesc].
#' @param initial.window (`numeric(1)`)\cr
#'   See [ResampleDesc].
#' @param skip (`integer(1)`)\cr
#'   See [ResampleDesc].
#' @template arg_measures
#' @param weights ([numeric])\cr
#'   Optional, non-negative case weight vector to be used during fitting.
#'   If given, must be of same length as observations in task and in corresponding order.
#'   Overwrites weights specified in the `task`.
#'   By default `NULL` which means no weights are used unless specified in the task.
#' @param models (`logical(1)`)\cr
#'   Should all fitted models be returned?
#'   Default is `FALSE`.
#' @param extract (`function`)\cr
#'   Function used to extract information from a fitted model during resampling.
#'   Is applied to every [WrappedModel] resulting from calls to [train]
#'   during resampling.
#'   Default is to extract nothing.
#' @template arg_keep_pred
#' @param ... (any)\cr
#'   Further hyperparameters passed to `learner`.
#' @template arg_showinfo
#' @return ([ResampleResult]).
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
  if (inherits(resampling, "ResampleDesc")) {
    resampling = makeResampleInstance(resampling, task = task)
  }
  assertClass(resampling, classes = "ResampleInstance")
  measures = checkMeasures(measures, task)
  if (!is.null(weights)) {
    assertNumeric(weights, len = n, any.missing = FALSE, lower = 0)
  }
  assertFlag(models)
  if (missing(extract)) {
    extract = function(model) {
    }
  } else {
    assertFunction(extract)
  }
  assertFlag(show.info)

  r = resampling$size
  if (n != r) {
    stop(stri_paste("Size of data set:", n, "and resampling instance:", r, "differ!", sep = " "))
  }

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

  if (show.info) {
    messagef("Resampling: %s", rin$desc$id)

    measure.lognames = extractSubList(measures, "id")
    # when predict on both some measure might be in there twice,
    # depending on aggregation fun, then we need to print each measure twice
    if (rin$desc$predict == "both") {
      id.train = which(vlapply(measures, function(x) "req.train" %in% x$aggr$properties))
      id.test = which(vlapply(measures, function(x) "req.test" %in% x$aggr$properties))
      measure.lognames = c(stri_paste(measure.lognames[id.train], "train", sep = "."),
        stri_paste(measure.lognames[id.test], "test", sep = "."))
    }
    printResampleFormatLine("Measures:", measure.lognames)
  }

  time1 = Sys.time()
  iter.results = parallelMap(doResampleIteration, seq_len(rin$desc$iters), level = "mlr.resample", more.args = more.args)
  time2 = Sys.time()
  runtime = as.numeric(difftime(time2, time1, units = "secs"))
  addClasses(
    mergeResampleResult(learner$id, task, iter.results, measures, rin, models, extract, keep.pred, show.info, runtime),
    "ResampleResult"
  )
}


# this wraps around calculateREsampleIterationResult and contains the subsetting for a specific fold i
doResampleIteration = function(learner, task, rin, i, measures, weights, model, extract, show.info) {
  setSlaveOptions()
  train.i = rin$train.inds[[i]]
  test.i = rin$test.inds[[i]]
  calculateResampleIterationResult(learner = learner, task = task, i = i, train.i = train.i, test.i = test.i, measures = measures,
    weights = weights, rdesc = rin$desc, model = model, extract = extract, show.info = show.info)
}


# Evaluate one train/test split of the resample function and get one or more performance values
calculateResampleIterationResult = function(learner, task, i, train.i, test.i, measures,
  weights, rdesc, model, extract, show.info) {

  err.msgs = c(NA_character_, NA_character_)
  err.dumps = list()
  m = train(learner, task, subset = train.i, weights = weights[train.i])
  if (isFailureModel(m)) {
    err.msgs[1L] = getFailureModelMsg(m)
    err.dumps$train = getFailureModelDump(m)
  }

  # does a measure require to calculate pred.train?
  ms.train = rep(NA, length(measures))
  ms.test = rep(NA, length(measures))
  pred.train = NULL
  pred.test = NULL
  pp = rdesc$predict
  train.task = task
  if (pp == "train") {
    lm = getLearnerModel(m)
    if ("BaseWrapper" %in% class(learner) && !is.null(lm$train.task)) {
      # the learner was wrapped in a sampling wrapper
      train.task = lm$train.task
      train.i = lm$subset
    }
    pred.train = predict(m, train.task, subset = train.i)
    if (!is.na(pred.train$error)) err.msgs[2L] = pred.train$error
    ms.train = performance(task = task, model = m, pred = pred.train, measures = measures)
    names(ms.train) = vcapply(measures, measureAggrName)
    err.dumps$predict.train = getPredictionDump(pred.train)
  } else if (pp == "test") {
    pred.test = predict(m, task, subset = test.i)
    if (!is.na(pred.test$error)) err.msgs[2L] = pred.test$error
    ms.test = performance(task = task, model = m, pred = pred.test, measures = measures)
    names(ms.test) = vcapply(measures, measureAggrName)
    err.dumps$predict.test = getPredictionDump(pred.test)
  } else { # "both"
    lm = getLearnerModel(m)
    if ("BaseWrapper" %in% class(learner) && !is.null(lm$train.task)) {
      # the learner was wrapped in a sampling wrapper
      train.task = lm$train.task
      train.i = lm$subset
    }
    pred.train = predict(m, train.task, subset = train.i)
    if (!is.na(pred.train$error)) err.msgs[2L] = pred.train$error
    ms.train = performance(task = task, model = m, pred = pred.train, measures = measures)
    names(ms.train) = vcapply(measures, measureAggrName)
    err.dumps$predict.train = getPredictionDump(pred.train)

    pred.test = predict(m, task, subset = test.i)
    if (!is.na(pred.test$error)) err.msgs[2L] = paste(err.msgs[2L], pred.test$error)
    ms.test = performance(task = task, model = m, pred = pred.test, measures = measures)
    names(ms.test) = vcapply(measures, measureAggrName)
    err.dumps$predict.test = getPredictionDump(pred.test)
  }
  if (!is.null(err.dumps$train)) {
    # if training was an error, these will just contain copies of the error dump
    # and confuse the user.
    err.dumps$predict.train = NULL
    err.dumps$predict.test = NULL
  }
  ex = extract(m)
  if (show.info) {
    idx.train = which(vlapply(measures, function(x) "req.train" %in% x$aggr$properties))
    idx.test = which(vlapply(measures, function(x) "req.test" %in% x$aggr$properties))
    ms.ids = extractSubList(measures, "id")
    if (pp == "both") {
      x = c(ms.train[idx.train], ms.test[idx.test])
      names(x) = c(stri_paste(ms.ids[idx.train], "train", sep = "."),
        stri_paste(ms.ids[idx.test], "test", sep = "."))
    } else {
      if (pp == "train") {
        x = ms.train[idx.train]
      } else {
        x = ms.test[idx.test]
      }
      names(x) = ms.ids
    }
    iter.message = sprintf("[Resample] iter %i:", i)
    printResampleFormatLine(iter.message, x)
  }
  list(
    measures.test = ms.test,
    measures.train = ms.train,
    model = if (model) m else NULL,
    pred.test = pred.test,
    pred.train = pred.train,
    err.msgs = err.msgs,
    err.dumps = err.dumps,
    extract = ex
  )
}


# Merge a list of train/test splits created by calculateResampleIterationResult to one resample result
mergeResampleResult = function(learner.id, task, iter.results, measures, rin,
  models, extract, keep.pred, show.info, runtime) {

  iters = length(iter.results)
  mids = vcapply(measures, function(m) m$id)

  ms.train = as.data.frame(extractSubList(iter.results, "measures.train", simplify = "rows"))

  ms.test = extractSubList(iter.results, "measures.test", simplify = FALSE)
  ms.test = as.data.frame(do.call(rbind, ms.test))

  preds.test = extractSubList(iter.results, "pred.test", simplify = FALSE)
  preds.train = extractSubList(iter.results, "pred.train", simplify = FALSE)
  pred = makeResamplePrediction(instance = rin, preds.test = preds.test, preds.train = preds.train, task.desc = getTaskDesc(task))

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

  err.dumps = extractSubList(iter.results, "err.dumps", simplify = FALSE)

  if (show.info) {
    # use measure ids for printing
    # aggr.out = aggr
    # names(aggr.out) = extractSubList(measures, "id")
    message("\n")
    messagef("Aggregated Result: %s", perfsToString(aggr))
    # last line break is there to seperate aggregated
    # results from objects returned by other functions (e.g. benchmark)
    message("\n")
  }

  if (!keep.pred) {
    pred = NULL
  }

  list(
    learner.id = learner.id,
    task.id = getTaskId(task),
    task.desc = getTaskDesc(task),
    measures.train = ms.train,
    measures.test = ms.test,
    aggr = aggr,
    pred = pred,
    models = if (models) lapply(iter.results, function(x) x$model) else NULL,
    err.msgs = err.msgs,
    err.dumps = err.dumps,
    extract = if (is.function(extract)) extractSubList(iter.results, "extract", simplify = FALSE) else NULL,
    runtime = runtime
  )
}
