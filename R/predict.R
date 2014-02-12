#' Predict new data.
#'
#' Predict the target variable of new data using a fitted model.
#' What is stored exactly in the [\code{\link{Prediction}}] object depends
#' on the \code{predict.type} setting of the \code{\link{Learner}}.
#' If \code{predict.type} was set to \dQuote{prob} probability thresholding
#' can be done calling the \code{\link{setThreshold}} function on the
#' prediction object.
#'
#' @param object [\code{\link{WrappedModel}}]\cr
#'   Wrapped model, result of \code{\link{train}}.
#' @param task [\code{\link{SupervisedTask}}]\cr
#'   The task. If this is passed, data from this task is predicted.
#' @param newdata [\code{data.frame}]\cr
#'   New observations which should be predicted.
#'   Pass this alternatively instead of \code{task}.
#' @param subset [\code{integer}]\cr
#'   Index vector to subset \code{task} or \code{newdata}.
#'   Default is all data.
#' @param ... [any]\cr
#'   Currently ignored.
#' @return [\code{\link{Prediction}}].
#' @method predict WrappedModel
#' @S3method predict WrappedModel
#' @note To extract probabilities use \code{\link{getProbabilities}}.
#' @examples
#' ## split iris data in training and test set
#' n <- nrow(iris)
#' mixed.set <- sample(1:n)
#' training.set <- mixed.set[1:(n/2)]
#' test.set <- mixed.set[(n/2 + 1):n]
#'
#' ## use linear discriminant analysis as learner for classification task
#' task <- makeClassifTask(data = iris, target = "Species")
#' learner <- makeLearner("classif.lda", method = "mle")
#' mod <- train(learner, task, subset = training.set)
#'
#' ## predict class labels for test data
#' pred <- predict(mod, newdata = iris[test.set,])
#' head(pred$data)
#'
#' ## predict now probabiliies instead of class labels
#' learner <- makeLearner("classif.lda", method = "mle", predict.type = "prob")
#' mod <- train(learner, task, subset = training.set)
#' pred <- predict(mod, newdata = iris[test.set, ])
#' head(pred$data)
predict.WrappedModel = function(object, task, newdata, subset, ...) {
  if (!xor(missing(task), missing(newdata)))
    stop("Pass either a task object or a newdata data.frame to predict, but not both!")
  checkArg(object, "WrappedModel")
  model = object
  learner = model$learner
  td = model$task.desc

  # FIXME: cleanup if cases
  if (missing(newdata)) {
    checkArg(task, "SupervisedTask")
    size = task$task.desc$size
  } else {
    checkArg(newdata, "data.frame")
    size = nrow(newdata)
    if (size == 0L)
      stop("newdata must be a data.frame with at least one row!")
    # FIXME check that data is of same structure?
  }
  if (missing(subset)) {
    subset = seq_len(size)
  } else {
    subset = convertIntegers(subset)
    # FIXME: min.len, lower, upper
    checkArg(subset, "integer", na.ok=FALSE)
  }
  if (missing(newdata)) {
    newdata = getTaskData(task, subset)
  } else {
    newdata = newdata[subset,,drop=FALSE]
  }

  # if we saved a model and loaded it later just for prediction this is necessary
  requireLearnerPackages(learner)
  t.col = which(colnames(newdata) %in% td$target)
  # get truth and drop target col, if target in newdata
  if (length(t.col)) {
    #FIXME this copies data
    truth = newdata[, t.col]
    newdata = newdata[, -t.col, drop=FALSE]
  } else {
    truth = NULL
  }

  # was there an error in building the model? --> return NAs
  if(inherits(model, "FailureModel")) {
    p = predict_nas(model, newdata)
    time.predict = NA_real_
  } else {
    #FIXME this copies newdata
    pars = list(
      .learner = learner,
      .model = model,
      .newdata = newdata
    )
    pars = c(pars, getHyperPars(learner, "predict"))
    debug.seed = getMlrOption("debug.seed", NULL)
    if(!is.null(debug.seed))
      set.seed(debug.seed)
    if(inherits(getLearnerModel(model), "NoFeaturesModel")) {
      p = predict_nofeatures(model, newdata)
      time.predict = 0
    } else {
      opt.ole = getMlrOption("on.learner.error")
      if (getMlrOption("show.learner.output"))
        fun1 = identity
      else
        fun1 = capture.output
      if (opt.ole == "stop")
        fun2 = identity
      else
        fun2 = function(x) try(x, silent=TRUE)
      old.warn.opt = getOption("warn")
      on.exit(options(warn = old.warn.opt))
      if (getMlrOption("on.learner.warning") == "quiet") {
        options(warn = -1L)
      }
      st = system.time(fun1(p <- fun2(do.call(predictLearner2, pars))), gcFirst = FALSE)
      time.predict = as.numeric(st[3L])
      # was there an error during prediction?
      if(is.error(p)) {
        if (opt.ole == "warn")
          warningf("Could not predict with learner %s: %s", learner$id, as.character(p))
        p = predict_nas(model, newdata)
        time.predict = NA_real_
      }
    }
  }
  if (missing(task))
    ids = NULL
  else
    ids = subset
  makePrediction(task.desc=td, id=ids, truth=truth,
    predict.type=learner$predict.type, y=p, time=time.predict)
}
