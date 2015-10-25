#' @title Classification via regression wrapper.
#'
#' @description
#' Builds a regression models that predict the probability for each class.
#'
#' Note that the predictions of the regression models are truncated at 0 and 1 when class probabilities are requested.
#'
#' @template arg_learner
#' @param predict.type [\code{character(1)}]\cr
#'   \dQuote{response} (= labels) or \dQuote{prob} (= probabilities and labels by selecting the ones with maximal probability).
#' @param ... [any]\cr
#'   Additional parameters passed down to the filter.
#' @template ret_learner
#' @export
#' @family wrapper
#' @examples
#' lrn = makeLearner("regr.rpart")
#' lrn = makeClassificationViaRegressionWrapper(lrn)
#' mod = train(lrn, iris.task, subset = 1:100)
#' predictions = predict(mod, newdata = iris[101:150, 1:4])
makeClassificationViaRegressionWrapper = function(learner, predict.type = "response", ...) {
  learner = checkLearner(learner, "regr")
  ddd = list(...)
  assertList(ddd, names = "named")

  lrn = makeLearnerBaseConstructor(classes = c("ClassificationViaRegressionWrapper", "Wrapper"),
    id = paste(learner$id, "as.classify", sep = "."),
    type = "classif",
    predict.type = predict.type,
    properties = c(learner$properties, "oneclass", "twoclass", "multiclass"),
    package = learner$package,
    par.set = list(),
    par.vals = list()
  )
  lrn$model.subclass = "ClassificationViaRegressionModel"
  lrn$next.learner = learner
  lrn$fix.factors.prediction = FALSE
  lrn$more.args = ddd
  lrn
}

#' @export
trainLearner.ClassificationViaRegressionWrapper = function(.learner, .task, .subset, .weights = NULL, ... ) {
  classes = getTaskClassLevels(.task)
  models = lapply(classes, function(class) {
    targetName = paste(class, "prob", sep = ".")
    data = getTaskData(.task)[, getTaskFeatureNames(.task), drop = FALSE]
    data[targetName] = as.numeric(getTaskTargets(.task) == class)
    regr.task = makeRegrTask(id = paste(getTaskId(.task), class, sep = "."),
      data = data,
      target = targetName,
      weights = .task$weights,
      blocking = .task$blocking)
    m = train(.learner$next.learner, regr.task, .subset, weights = .weights)
    m$class = class
    m
  })
  cm = makeChainModel(next.model = models, cl = "ClassificationViaRegressionModel")
  return(cm)
}

#' @export
predictLearner.ClassificationViaRegressionWrapper = function(.learner, .model, .newdata, ...) {
  models = getLearnerModel(.model, more.unwrap = FALSE)$next.model
  p = asMatrixCols(lapply(models, function(m) {
    predict(m, newdata = .newdata, ...)$data$response
  }))
  colnames(p) = sapply(models, function(m) m$class)

  if (.learner$predict.type == "response") {
    factor(colnames(p)[apply(p, 1, which.max)], levels = .model$task.desc$class.levels)
  } else {
    p[p < 0] = 0
    p[p > 1] = 1
    p
  }
}
