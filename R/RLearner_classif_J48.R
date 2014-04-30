# checked props
#' @S3method makeRLearner classif.J48
makeRLearner.classif.J48 = function() {
  makeRLearnerClassif(
    cl = "classif.J48",
    package = "RWeka",
    par.set = makeParamSet(
      makeLogicalLearnerParam(id="U"),
      makeLogicalLearnerParam(id="O"),
      makeNumericLearnerParam(id="C", default=0.25, lower=0),
      makeIntegerLearnerParam(id="M", default=2L, lower=1L),
      makeLogicalLearnerParam(id="R"),
      makeIntegerLearnerParam(id="N", default=3L, lower=2L),
      makeLogicalLearnerParam(id="B"),
      makeLogicalLearnerParam(id="S"),
      makeLogicalLearnerParam(id="L"),
      makeLogicalLearnerParam(id="A"),
      makeLogicalLearnerParam(id="J")
    ),
    twoclass = TRUE,
    multiclass = TRUE,
    missings = TRUE,
    numerics = TRUE,
    factors = TRUE,
    prob = TRUE
  )
}

#' @S3method trainLearner classif.J48
trainLearner.classif.J48 = function(.learner, .task, .subset, .weights,  ...) {
  ctrl = Weka_control(..., Q=as.integer(runif(1, min=-.Machine$integer.max, max=.Machine$integer.max)))
  f = getTaskFormulaAsString(.task)
  J48(as.formula(f), data=getTaskData(.task, .subset), control=ctrl, na.action=na.pass)
}

#' @S3method predictLearner classif.J48
predictLearner.classif.J48 = function(.learner, .model, .newdata, ...) {
  type = switch(.learner$predict.type, prob="prob", "class")
  predict(.model$learner.model, newdata=.newdata, type=type, ...)
}
