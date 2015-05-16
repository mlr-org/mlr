#' @export
makeRLearner.classif.rFerns = function() {
  makeRLearnerClassif(
    cl = "classif.rFerns",
    package = "rFerns",
    par.set = makeParamSet(
      makeIntegerLearnerParam(id = "depth", default = 5L),
      makeIntegerLearnerParam(id = "ferns", default = 1000L)
    ),
    properties = c("numerics", "twoclass", "multiclass", "factors", "ordered"),
    name = "Random ferns",
    short.name = "rFerns",
    note = ""
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
