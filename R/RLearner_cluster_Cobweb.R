#' @export
makeRLearner.cluster.Cobweb = function() {
  makeRLearnerCluster(
    cl = "cluster.Cobweb",
    package = "RWeka",
    par.set = makeParamSet(
      makeNumericLearnerParam(id = "A", default = 1, lower = 0),
      makeNumericLearnerParam(id = "C", default = 0.002, lower = 0),
      makeIntegerLearnerParam(id = "S", default = 42L, lower = 1L)
    ),
    properties = "numerics",
    name = "Cobweb Clustering Algorithm",
    short.name = "cobweb",
    callees = c("Cobweb", "Weka_control")
  )
}

#' @export
trainLearner.cluster.Cobweb = function(.learner, .task, .subset, .weights = NULL, ...) {
  ctrl = RWeka::Weka_control(...)
  RWeka::Cobweb(getTaskData(.task, .subset), control = ctrl)
}

#' @export
predictLearner.cluster.Cobweb = function(.learner, .model, .newdata, ...) {
  # RWeka returns cluster indices (i.e. starting from 0, which some tools don't like
  as.integer(predict(.model$learner.model, .newdata, ...)) + 1L
}
