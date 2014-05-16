#' @S3method makeRLearner classif.IBk
makeRLearner.classif.IBk = function() {
  makeRLearnerClassif(
    cl = "classif.IBk",
    package = "RWeka",
    par.set = makeParamSet(
      makeLogicalLearnerParam(id = "I"),
      makeLogicalLearnerParam(id = "F"),
      makeIntegerLearnerParam(id = "K", lower = 1L, default = 1L),
      makeLogicalLearnerParam(id = "E"),
      makeIntegerLearnerParam(id = "W", lower = 0L),
      makeLogicalLearnerParam(id = "X"),
      makeUntypedLearnerParam(id = "A", default = "weka.core.neighboursearch.LinearNNSearch")
    ),
    twoclass = TRUE,
    multiclass = TRUE,
    missings = TRUE,
    numerics = TRUE,
    factors = TRUE,
    prob = TRUE
  )
}

#' @S3method trainLearner classif.IBk
trainLearner.classif.IBk = function(.learner, .task, .subset, .weights = NULL,  ...) {
  ctrl = Weka_control(...)
  IBk(getTaskFormula(.task), data = getTaskData(.task, .subset), control = ctrl, na.action = na.pass)
}

#' @S3method predictLearner classif.IBk
predictLearner.classif.IBk = function(.learner, .model, .newdata, ...) {
  type = switch(.learner$predict.type, prob = "prob", "class")
  predict(.model$learner.model, newdata = .newdata, type = type, ...)
}
