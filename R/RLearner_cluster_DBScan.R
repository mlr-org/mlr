#' @export
makeRLearner.cluster.DBScan = function() {
  makeRLearnerCluster(
    cl = "cluster.DBScan",
    package = "RWeka",
    par.set = makeParamSet(
      makeUntypedLearnerParam(id = "D", default = "weka.clusterers.forOPTICSAndDBScan.DataObjects.EuclideanDataObject"),
      makeNumericLearnerParam(id = "E", default = 0.9, lower = 0),
      makeUntypedLearnerParam(id = "I", default = "weka.clusterers.forOPTICSAndDBScan.Databases.SequentialDatabase"),
      makeIntegerLearnerParam(id = "M", default = 6L, lower = 1L)
    ),
    properties = c("numerics")
  )
}

#' @export
trainLearner.cluster.DBScan = function(.learner, .task, .subset, .weights = NULL,  ...) {
  ctrl = Weka_control(...)
  DBScan(getTaskData(.task, .subset), control = ctrl)
}

#' @export
predictLearner.cluster.DBScan = function(.learner, .model, .newdata, ...) {
  # DBScan returns cluster indices (i.e. starting from 0, which some tools don't like
  predict(.model$learner.model, .newdata, ...) + 1
}

