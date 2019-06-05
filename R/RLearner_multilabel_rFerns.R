#' @export
makeRLearner.multilabel.rFerns = function() {
  makeRLearnerMultilabel(
    cl = "multilabel.rFerns",
    package = "rFerns",
    par.set = makeParamSet(
      makeIntegerLearnerParam(id = "depth", default = 5L),
      makeIntegerLearnerParam(id = "ferns", default = 1000L)
    ),
    properties = c("numerics", "factors", "ordered"),
    name = "Random ferns",
    short.name = "rFerns",
    callees = "rFerns"
  )
}

#' @export
trainLearner.multilabel.rFerns = function(.learner, .task, .subset, .weights = NULL, ...) {
  d = getTaskData(.task, .subset, target.extra = TRUE)
  rFerns::rFerns(x = d$data, y = as.matrix(d$target), ...)
}

#' @export
predictLearner.multilabel.rFerns = function(.learner, .model, .newdata, ...) {
  as.matrix(predict(.model$learner.model, .newdata, ...))
}
