#' @S3method makeRLearner classif.PART
makeRLearner.classif.PART = function() {
  makeRLearnerClassif(
   cl = "classif.PART",
   package = "RWeka",
   par.set = makeParamSet(
     makeNumericLearnerParam(id="C", default=0.25, lower=0),
     makeIntegerLearnerParam(id="M", default=2L, lower=1L),
     makeLogicalLearnerParam(id="R"),
     makeIntegerLearnerParam(id="N", default=3L, lower=2L),
     makeLogicalLearnerParam(id="B"),
     makeLogicalLearnerParam(id="U"),
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

#' @S3method trainLearner classif.PART
trainLearner.classif.PART = function(.learner, .task, .subset, .weights,  ...) {
  f = getTaskFormula(.task)
  ctrl = Weka_control(..., Q=as.integer(runif(1L, min=-.Machine$integer.max, max=.Machine$integer.max)))
  PART(f, data=getTaskData(.task, .subset), control=ctrl, na.action=na.pass)
}

#' @S3method predictLearner classif.PART
predictLearner.classif.PART = function(.learner, .model, .newdata, ...) {
  type = switch(.learner$predict.type, prob="prob", "class")
  predict(.model$learner.model, newdata=.newdata, type=type, ...)
}
