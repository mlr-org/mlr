#' @export
makeRLearner.classif.rpart = function() {
  makeRLearnerClassif(
    cl = "classif.rpart",
    package = "rpart",
    par.set = makeParamSet(
      makeIntegerLearnerParam(id = "minsplit", default = 20L, lower = 1L),
      makeIntegerLearnerParam(id = "minbucket", lower = 1L),
      makeNumericLearnerParam(id = "cp", default = 0.01, lower = 0, upper = 1),
      makeIntegerLearnerParam(id = "maxcompete", default = 4L, lower = 0L),
      makeIntegerLearnerParam(id = "maxsurrogate", default = 5L, lower = 0L),
      makeDiscreteLearnerParam(id = "usesurrogate", default = 2L, values = 0:2),
      makeDiscreteLearnerParam(id = "surrogatestyle", default = 0L, values = 0:1),
      # we use 30 as upper limit, see docs of rpart.control
      makeIntegerLearnerParam(id = "maxdepth", default = 30L, lower = 1L, upper = 30L),
      makeIntegerLearnerParam(id = "xval", default = 10L, lower = 0L, tunable = FALSE),
      makeUntypedLearnerParam(id = "parms")
    ),
    par.vals = list(xval = 0L),
    properties = c("twoclass", "multiclass", "missings", "numerics", "factors", "ordered", "prob", "weights", "featimp"),
    name = "Decision Tree",
    short.name = "rpart",
    note = "`xval` has been set to `0` by default for speed.",
    callees = c("rpart", "rpart.control")
  )
}

#' @export
trainLearner.classif.rpart = function(.learner, .task, .subset, .weights = NULL, ...) {
  d = getTaskData(.task, .subset)
  if (is.null(.weights)) {
    f = getTaskFormula(.task)
    rpart::rpart(f, data = d, ...)
  } else {
    f = getTaskFormula(.task)
    rpart::rpart(f, data = d, weights = .weights, ...)
  }
}

#' @export
predictLearner.classif.rpart = function(.learner, .model, .newdata, ...) {
  type = switch(.learner$predict.type, prob = "prob", "class")
  predict(.model$learner.model, newdata = .newdata, type = type, ...)
}

#' @export
getFeatureImportanceLearner.classif.rpart = function(.learner, .model, ...) {
  mod = getLearnerModel(.model, more.unwrap = TRUE)
  mod$variable.importance
}
