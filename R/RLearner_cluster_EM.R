#' @export
makeRLearner.cluster.EM = function() {
  makeRLearnerCluster(
    cl = "cluster.EM",
    package = "RWeka",
    par.set = makeParamSet(
      makeIntegerLearnerParam(id = "I", default = 100L, lower = 1L),
      makeNumericLearnerParam(id = "ll-cv", default = 1e-6, lower = 1e-6),
      makeNumericLearnerParam(id = "ll-iter", default = 1e-6, lower = 1e-6),
      makeNumericLearnerParam(id = "M", default = 1e-6, lower = 1e-6),
      makeIntegerLearnerParam(id = "max", default = -1L, lower = -1L),
      makeIntegerLearnerParam(id = "N", default = -1L, lower = -1L),
      makeIntegerLearnerParam(id = "num-slots", default = 1L, lower = 1L),
      makeIntegerLearnerParam(id = "S", default = 100L, lower = 0L),
      makeIntegerLearnerParam(id = "X", default = 10L, lower = 1L),
      makeIntegerLearnerParam(id = "K", default = 10L, lower = 1L),
      makeLogicalLearnerParam(id = "V", default = FALSE, tunable = FALSE),
      makeLogicalLearnerParam(id = "output-debug-info", default = FALSE, tunable = FALSE)
    ),
    properties = "numerics",
    name = "Expectation-Maximization Clustering",
    short.name = "em",
    callees = c("make_Weka_clusterer", "Weka_control")
  )
}

#' @export
trainLearner.cluster.EM = function(.learner, .task, .subset, .weights = NULL, ...) {
  ctrl = RWeka::Weka_control(...)
  RWeka::make_Weka_clusterer("weka/clusterers/EM")(getTaskData(.task, .subset), control = ctrl)
}

#' @export
predictLearner.cluster.EM = function(.learner, .model, .newdata, ...) {
  # EM returns cluster indices (i.e. starting from 0, which some tools don't like
  as.integer(predict(.model$learner.model, .newdata, ...)) + 1L
}
