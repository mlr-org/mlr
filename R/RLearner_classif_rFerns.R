#' @export
makeRLearner.classif.rFerns = function() {
  makeRLearnerClassif(
    cl = "classif.rFerns",
    package = "rFerns",
    par.set = makeParamSet(
      makeIntegerLearnerParam(id = "depth", default = 5L),
      makeIntegerLearnerParam(id = "ferns", default = 1000L),
      makeLogicalLearnerParam(id = "importance", default = FALSE, tunable = FALSE),
      makeIntegerLearnerParam(id = "reportErrorEvery", default = 0L, lower = 0L, tunable = FALSE),
      makeLogicalLearnerParam(id = "saveErrorPropagation", default = FALSE, tunable = FALSE),
      makeLogicalLearnerParam(id = "saveForest", default = TRUE, tunable = FALSE)
    ),
    properties = c("numerics", "twoclass", "multiclass", "factors", "ordered", "oobpreds"),
    name = "Random ferns",
    short.name = "rFerns",
    callees = "rFerns"
  )
}

#' @export
trainLearner.classif.rFerns = function(.learner, .task, .subset, .weights = NULL, ...) {
  d = getTaskData(.task, .subset, target.extra = TRUE)
  rFerns::rFerns(x = d$data, y = d$target, ...)
}

#' @export
predictLearner.classif.rFerns = function(.learner, .model, .newdata, ...) {
  predict(.model$learner.model, .newdata, ...)
}

#' @export
getOOBPredsLearner.classif.rFerns = function(.learner, .model) {
  getLearnerModel(.model, more.unwrap = TRUE)$oobPreds
}
