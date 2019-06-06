#' @export
makeRLearner.classif.pamr = function() {
  makeRLearnerClassif(
    cl = "classif.pamr",
    package = "pamr",
    par.set = makeParamSet(
      makeNumericVectorLearnerParam(id = "threshold"),
      makeIntegerLearnerParam(id = "n.threshold", default = 30L),
      makeLogicalLearnerParam(id = "scale.sd", default = TRUE),
      makeNumericVectorLearnerParam(id = "threshold.scale"),
      makeNumericVectorLearnerParam(id = "se.scale"),
      makeNumericLearnerParam(id = "offset.percent", default = 50, lower = 0, upper = 100),
      makeUntypedLearnerParam(id = "hetero"),
      makeNumericVectorLearnerParam(id = "prior", lower = 0, upper = 1),
      makeLogicalLearnerParam(id = "remove.zeros", default = TRUE),
      makeDiscreteLearnerParam(id = "sign.contrast", default = "both", values = c("both", "negative", "positive")),
      # we cannot the use the orginal argument name 'threshold', because it's already used
      makeNumericLearnerParam(id = "threshold.predict", default = 1, when = "predict")
      # FIXME threshold in pamr.predict() seems to have no default. If it has 1 as default, par.vals is redundant
    ),
    par.vals = list(threshold.predict = 1),
    properties = c("numerics", "twoclass", "prob"),
    name = "Nearest shrunken centroid",
    short.name = "pamr",
    note = "Threshold for prediction (`threshold.predict`) has been set to `1` by default.",
    callees = c("pamr.train", "pamr.predict")
  )
}

#' @export
trainLearner.classif.pamr = function(.learner, .task, .subset, .weights = NULL, ...) {
  d = getTaskData(.task, .subset, target.extra = TRUE)
  pamr::pamr.train(data = list(x = t(d$data), y = d$target), ...)
}

#' @export
predictLearner.classif.pamr = function(.learner, .model, .newdata, threshold.predict, ...) {
  type = ifelse(.learner$predict.type == "prob", "posterior", "class")
  pamr::pamr.predict(.model$learner.model, t(.newdata), threshold = threshold.predict, type = type, ...)
}
