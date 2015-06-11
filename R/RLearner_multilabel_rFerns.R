#' @export
makeRLearner.multilabel.rFerns = function() {
  makeRLearnerMultilabel(
    cl = "multilabel.rFerns",
    package = "rFerns",
    par.set = makeParamSet(
      makeIntegerLearnerParam(id = "depth", default = 5L),
      makeIntegerLearnerParam(id = "ferns", default = 1000L)
    ),
    properties = c("numerics", "oneclass", "twoclass", "factors", "ordered"),
    name = "Random ferns",
    short.name = "rFerns",
    note = ""
  )
}

#' @export
trainLearner.multilabel.rFerns = function(.learner, .task, .subset, .weights = NULL, ...) {
  d = getTaskData(.task, .subset, target.extra = TRUE)
  d$target = try(as.matrix(apply(d$target,2,as.logical)))
  rFerns::rFerns(x = d$data, y = d$target, ...)
}

#' @export
predictLearner.multilabel.rFerns = function(.learner, .model, .newdata, ...) {
  predict(.model$learner.model, .newdata, ...)
}
