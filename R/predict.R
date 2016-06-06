#' @title Predict new data.
#'
#' @description
#' Predict the target variable of new data using a fitted model.
#' What is stored exactly in the [\code{\link{Prediction}}] object depends
#' on the \code{predict.type} setting of the \code{\link{Learner}}.
#' If \code{predict.type} was set to \dQuote{prob} probability thresholding
#' can be done calling the \code{\link{setThreshold}} function on the
#' prediction object.
#'
#' The row names of the input \code{task} or \code{newdata} are preserved in the output.
#'
#' @param object [\code{\link{WrappedModel}}]\cr
#'   Wrapped model, result of \code{\link{train}}.
#' @param task [\code{\link{Task}}]\cr
#'   The task. If this is passed, data from this task is predicted.
#' @param newdata [\code{data.frame}]\cr
#'   New observations which should be predicted.
#'   Pass this alternatively instead of \code{task}.
#' @param subset [\code{integer} | \code{integer}]\cr
#'   An index vector specifying the training cases to be used for fitting.
#'   By default the complete data set is used.
#'   Logical vectors will be transformed to integer with \code{\link[base]{which}}.
#' @param ... [any]\cr
#'   Currently ignored.
#' @return [\code{\link{Prediction}}].
#' @family predict
#' @export
#' @examples
#' # train and predict
#' train.set = seq(1, 150, 2)
#' test.set = seq(2, 150, 2)
#' model = train("classif.lda", iris.task, subset = train.set)
#' p = predict(model, newdata = iris, subset = test.set)
#' print(p)
#' predict(model, task = iris.task, subset = test.set)
#'
#' # predict now probabiliies instead of class labels
#' lrn = makeLearner("classif.lda", predict.type = "prob")
#' model = train(lrn, iris.task, subset = train.set)
#' p = predict(model, task = iris.task, subset = test.set)
#' print(p)
#' getPredictionProbabilities(p)
predict.WrappedModel = function(object, task, newdata, subset, ...) {
  if (!xor(missing(task), missing(newdata)))
    stop("Pass either a task object or a newdata data.frame to predict, but not both!")
  assertClass(object, classes = "WrappedModel")
  model = object
  learner = model$learner
  td = model$task.desc

  # FIXME: cleanup if cases
  if (missing(newdata)) {
    assertClass(task, classes = "Task")
    size = getTaskSize(task)
  } else {
    assertDataFrame(newdata, min.rows = 1L)
    size = nrow(newdata)
  }
  if (missing(subset)) {
    subset = seq_len(size)
  } else {
    if (is.logical(subset))
      subset = which(subset)
    else
      subset = asInteger(subset, min.len = 1L, any.missing = FALSE, lower = 1L, upper = size)
  }
  if (missing(newdata)) {
    newdata = getTaskData(task, subset)
  } else {
    newdata = newdata[subset,, drop = FALSE]
  }

  # if we saved a model and loaded it later just for prediction this is necessary
  requireLearnerPackages(learner)
  t.col = match(td$target, colnames(newdata))

  # get truth and drop target col, if target in newdata
  if (!all(is.na(t.col))) {
    if (length(t.col) > 1L && anyMissing(t.col))
      stop("Some but not all target columns found in data")
    truth = newdata[, t.col, drop = TRUE]
    if (is.list(truth))
      truth = data.frame(truth)
    newdata = newdata[, -t.col, drop = FALSE]
  } else {
    truth = NULL
  }

  # was there an error in building the model? --> return NAs
  if (isFailureModel(model)) {
    p = predictFailureModel(model, newdata)
    time.predict = NA_real_
  } else {
    #FIXME: this copies newdata
    pars = list(
      .learner = learner,
      .model = model,
      .newdata = newdata
    )
    pars = c(pars, getHyperPars(learner, c("predict", "both")))
    debug.seed = getMlrOption("debug.seed", NULL)
    if (!is.null(debug.seed))
      set.seed(debug.seed)
    opts = getLearnerOptions(learner, c("show.learner.output", "on.learner.error", "on.learner.warning"))
    fun1 = if (opts$show.learner.output) identity else capture.output
    fun2 = if (opts$on.learner.error == "stop") identity else function(x) try(x, silent = TRUE)
    if (opts$on.learner.warning == "quiet") {
      old.warn.opt = getOption("warn")
      on.exit(options(warn = old.warn.opt))
      options(warn = -1L)
    }
    st = system.time(fun1(p <- fun2(do.call(predictLearner2, pars))), gcFirst = FALSE)
    time.predict = as.numeric(st[3L])
    # was there an error during prediction?
    if (is.error(p)) {
      if (opts$on.learner.warning == "warn")
        warningf("Could not predict with learner %s: %s", learner$id, as.character(p))
      p = predictFailureModel(model, newdata)
      time.predict = NA_real_
    }
  }
  if (missing(task))
    ids = NULL
  else
    ids = subset
  makePrediction(task.desc = td, row.names = rownames(newdata), id = ids, truth = truth,
    predict.type = learner$predict.type, predict.threshold = learner$predict.threshold, y = p, time = time.predict)
}
