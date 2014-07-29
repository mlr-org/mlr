#' @export
makeRLearner.cluster.SimpleKMeans = function() {
  makeRLearnerCluster(
    cl = "cluster.SimpleKMeans",
    package = "RWeka",
    par.set = makeParamSet(
      makeUntypedLearnerParam(id = "A", default = "weka.core.EuclideanDistance"),
      makeLogicalLearnerParam(id = "C"),
      makeLogicalLearnerParam(id = "fast"),
      makeIntegerLearnerParam(id = "I", default = 100L, lower = 1L),
      makeIntegerLearnerParam(id = "init", default = 0L, lower = 0L, upper = 3L),
      makeLogicalLearnerParam(id = "M"),
      makeIntegerLearnerParam(id = "max-candidates", default = 100L, lower = 1L),
      makeIntegerLearnerParam(id = "min-density", default = 2L, lower = 1L),
      makeIntegerLearnerParam(id = "N", default = 2L, lower = 1L),
      makeIntegerLearnerParam(id = "num-slots", default = 1L, lower = 1L),
      makeLogicalLearnerParam(id = "O"),
      makeIntegerLearnerParam(id = "periodic-pruning", default = 10000L, lower = 1L),
      makeIntegerLearnerParam(id = "S", default = 10L, lower = 0L),
      makeNumericLearnerParam(id = "t2", default = -1),
      makeNumericLearnerParam(id = "t1", default = -1.5),
      makeLogicalLearnerParam(id = "V")
    ),
    properties = c("numerics"),
    name = "cluster.SimpleKMeans",
    short.name = "SimpleKMeans",
    note = ""
  )
}

#' @export
trainLearner.cluster.SimpleKMeans = function(.learner, .task, .subset, .weights = NULL,  ...) {
  ctrl = Weka_control(...)
  SimpleKMeans(getTaskData(.task, .subset), control = ctrl)
}

#' @export
predictLearner.cluster.SimpleKMeans = function(.learner, .model, .newdata, ...) {
  # SimpleKMeans returns cluster indices (i.e. starting from 0, which some tools don't like
  predict(.model$learner.model, .newdata, ...) + 1
}

