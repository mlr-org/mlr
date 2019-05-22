#' @export
makeRLearner.cluster.XMeans = function() {
  makeRLearnerCluster(
    cl = "cluster.XMeans",
    package = "RWeka",
    par.set = makeParamSet(
      makeNumericLearnerParam(id = "B", default = 1, lower = 0),
      makeNumericLearnerParam(id = "C", default = 0, lower = 0),
      makeUntypedLearnerParam(id = "D", default = "weka.core.EuclideanDistance"),
      makeIntegerLearnerParam(id = "H", default = 4L, lower = 1L),
      makeIntegerLearnerParam(id = "I", default = 1L, lower = 1L),
      makeIntegerLearnerParam(id = "J", default = 1000L, lower = 1L),
      makeUntypedLearnerParam(id = "K", default = ""),
      makeIntegerLearnerParam(id = "L", default = 2L, lower = 1L),
      makeIntegerLearnerParam(id = "M", default = 1000L, lower = 1L),
      makeIntegerLearnerParam(id = "S", default = 10L, lower = 1L),
      makeIntegerLearnerParam(id = "U", default = 0L, lower = 0L),
      makeLogicalLearnerParam(id = "use-kdtree", default = FALSE),
      makeUntypedLearnerParam(id = "N"),
      makeUntypedLearnerParam(id = "O"),
      makeUntypedLearnerParam(id = "Y"),
      makeLogicalLearnerParam(id = "output-debug-info", default = FALSE, tunable = FALSE)
    ),
    properties = "numerics",
    name = "XMeans (k-means with automatic determination of k)",
    short.name = "xmeans",
    note = "You may have to install the XMeans Weka package: `WPM('install-package', 'XMeans')`.",
    callees = c("XMeans", "Weka_control")
  )
}

#' @export
trainLearner.cluster.XMeans = function(.learner, .task, .subset, .weights = NULL, ...) {
  ctrl = RWeka::Weka_control(...)
  RWeka::XMeans(getTaskData(.task, .subset), control = ctrl)
}

#' @export
predictLearner.cluster.XMeans = function(.learner, .model, .newdata, ...) {
  # XMeans returns cluster indices (i.e. starting from 0, which some tools don't like
  as.integer(predict(.model$learner.model, .newdata, ...)) + 1L
}
