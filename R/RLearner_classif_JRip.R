#' @S3method makeRLearner classif.JRip
makeRLearner.classif.JRip = function() {
  makeRLearnerClassif(
    cl = "classif.JRip",
    package = "RWeka",
    par.set = makeParamSet(
      makeIntegerLearnerParam(id="F", default=3L, lower=2L),
      makeNumericLearnerParam(id="N", default=2, lower=0),
      makeIntegerLearnerParam(id="O", default=2L, lower=1L),
      makeLogicalLearnerParam(id="E", default=FALSE),
      makeLogicalLearnerParam(id="P", default=FALSE)
    ),
    oneclass = FALSE,
    twoclass = TRUE,
    multiclass = TRUE,
    missings = TRUE,
    numerics = TRUE,
    factors = TRUE,
    prob = TRUE,
    weights = FALSE
  )
}

#' @S3method trainLearner classif.JRip
trainLearner.classif.JRip = function(.learner, .task, .subset, .weights,  ...) {
  f = getTaskFormula(.task)
  ctrl = Weka_control(..., S=as.integer(runif(1, min=-.Machine$integer.max, max=.Machine$integer.max)))
  JRip(f, data=getTaskData(.task, .subset), control=ctrl, na.action=na.pass)
}

#' @S3method predictLearner classif.JRip
predictLearner.classif.JRip = function(.learner, .model, .newdata, ...) {
  type = switch(.learner$predict.type, prob="prob", "class")
  predict(.model$learner.model, newdata=.newdata, type=type, ...)
}
