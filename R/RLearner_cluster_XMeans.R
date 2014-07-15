#' @export
makeRLearner.cluster.XMeans = function() {
  makeRLearnerCluster(
    cl = "cluster.XMeans",
    package = "RWeka",
    par.set = makeParamSet(
      makeIntegerLearnerParam(id = "I", default = 1L, lower = 1L)
      # FIXME: add missing params
    ),
    properties = c("missings", "numerics", "factors")
  )
}

#' @export
trainLearner.cluster.XMeans = function(.learner, .task, .subset, .weights = NULL,  ...) {
  ctrl = Weka_control(...)
  XMeans(getTaskData(.task, .subset), control = ctrl)
}

#' @export
predictLearner.cluster.XMeans = function(.learner, .model, .newdata, ...) {
  predict(.model$learner.model, newdata = .newdata, ...)
}

