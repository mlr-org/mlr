#' @title Predict new data.
#'
#' @description
#' Predict the target variable of new data using a fitted model.
#' What is stored exactly in the ([Prediction]) object depends
#' on the `predict.type` setting of the [Learner].
#' If `predict.type` was set to \dQuote{prob} probability thresholding
#' can be done calling the [setThreshold] function on the
#' prediction object.
#'
#' The row names of the input `task` or `newdata` are preserved in the output.
#'
#' @param object ([WrappedModel])\cr
#'   Wrapped model, result of [train].
#' @param task ([Task])\cr
#'   The task. If this is passed, data from this task is predicted.
#' @param newdata ([data.frame])\cr
#'   New observations which should be predicted.
#'   Pass this alternatively instead of `task`.
#' @template arg_subset
#' @param ... (any)\cr
#'   Currently ignored.
#' @return ([Prediction]).
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
predict.WrappedModel = function(object, task, newdata, subset = NULL, ...) {

  if (!xor(missing(task), missing(newdata))) {
    stop("Pass either a task object or a newdata data.frame to predict, but not both!")
  }
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
    if (class(newdata)[1] != "data.frame") {
      warningf("Provided data for prediction is not a pure data.frame but from class %s, hence it will be converted.", class(newdata)[1])
      newdata = as.data.frame(newdata)
    }
    size = nrow(newdata)
  }
  subset = checkTaskSubset(subset, size)

  if (missing(newdata)) {
    # if learner does not support functional, we convert to df cols
    if (hasLearnerProperties(object$learner, "functionals") ||
      hasLearnerProperties(object$learner, "single.functional")) {
      newdata = getTaskData(task, subset, functionals.as = "matrix")
    } else {
      newdata = getTaskData(task, subset, functionals.as = "dfcols")
    }
  } else {
    newdata = newdata[subset, , drop = FALSE]
  }

  # if we saved a model and loaded it later just for prediction this is necessary
  requireLearnerPackages(learner)
  t.col = match(td$target, colnames(newdata))

  # get truth and drop target col, if target in newdata
  if (!all(is.na(t.col))) {
    if (length(t.col) > 1L && anyMissing(t.col)) {
      stop("Some but not all target columns found in data")
    }
    truth = newdata[, t.col, drop = TRUE]
    if (is.list(truth)) {
      truth = data.frame(truth)
    }
    newdata = newdata[, -t.col, drop = FALSE]
  } else {
    truth = NULL
  }

  error = NA_character_
  # default to NULL error dump
  dump = NULL
  # was there an error in building the model? --> return NAs
  if (isFailureModel(model)) {
    p = predictFailureModel(model, newdata)
    time.predict = NA_real_
    dump = getFailureModelDump(model)
  } else {
    # FIXME: this copies newdata
    pars = list(
      .learner = learner,
      .model = model,
      .newdata = newdata
    )
    pars = c(pars, getHyperPars(learner, c("predict", "both")))
    debug.seed = getMlrOption("debug.seed", NULL)
    if (!is.null(debug.seed)) {
      set.seed(debug.seed)
    }
    opts = getLearnerOptions(learner, c("show.learner.output", "on.learner.error", "on.learner.warning", "on.error.dump"))
    fun1 = if (opts$show.learner.output) identity else capture.output
    fun2 = if (opts$on.learner.error == "stop") identity else function(x) try(x, silent = TRUE)
    fun3 = if (opts$on.learner.error == "stop" || !opts$on.error.dump) {
      identity
    } else {
      function(x) {
        withCallingHandlers(x, error = function(c) utils::dump.frames())
      }
    }
    if (opts$on.learner.warning == "quiet") {
      old.warn.opt = getOption("warn")
      on.exit(options(warn = old.warn.opt))
      options(warn = -1L)
    }

    time.predict = measureTime(fun1({
      p = fun2(fun3(do.call(predictLearner2, pars)))
    }))

    # was there an error during prediction?
    if (is.error(p)) {
      if (opts$on.learner.error == "warn") {
        warningf("Could not predict with learner %s: %s", learner$id, as.character(p))
      }
      error = as.character(p)
      p = predictFailureModel(model, newdata)
      time.predict = NA_real_
      if (opts$on.error.dump) {
        dump = addClasses(get("last.dump", envir = .GlobalEnv), "mlr.dump")
      }
    }
  }
  if (missing(task)) {
    ids = NULL
  } else {
    ids = subset
  }
  makePrediction(task.desc = td, row.names = rownames(newdata), id = ids, truth = truth,
    predict.type = learner$predict.type, predict.threshold = learner$predict.threshold, y = p, time = time.predict, error = error, dump = dump)
}
