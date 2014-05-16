#' @S3method makeRLearner classif.OneR
makeRLearner.classif.OneR = function() {
  makeRLearnerClassif(
    cl = "classif.OneR",
    package = "RWeka",
    par.set = makeParamSet(
      makeIntegerLearnerParam(id="B", default=6L, lower=1L)
    ),
    twoclass = TRUE,
    multiclass = TRUE,
    missings = TRUE,
    numerics = TRUE,
    factors = TRUE,
    prob = TRUE
  )
}

#' @S3method trainLearner classif.OneR
trainLearner.classif.OneR = function(.learner, .task, .subset, .weights = NULL,  ...) {
  f = getTaskFormula(.task)
  ctrl = Weka_control(...)
	OneR(f, data=getTaskData(.task, .subset), control=ctrl, na.action=na.pass)
}

#' @S3method makeRLearner classif.OneR
predictLearner.classif.OneR = function(.learner, .model, .newdata, ...) {
  type = switch(.learner$predict.type, prob="prob", "class")
  predict(.model$learner.model, newdata=.newdata, type=type, ...)
}
