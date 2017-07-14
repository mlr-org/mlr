#' @export
makeRLearner.cluster.DBScan = function() {
  makeRLearnerCluster(
    cl = "cluster.DBScan",
    package = "RWeka",
    par.set = makeParamSet(
      makeNumericLearnerParam(id = "E", default = 0.9, lower = 1e-6),
      makeIntegerLearnerParam(id = "M", default = 6, lower = 1),
      makeUntypedLearnerParam(id = "I", default = "weka.clusterers.forOPTICSAndDBScan.Databases.SequentialDatabase"),
      makeUntypedLearnerParam(id = "D", default = "weka.clusterers.forOPTICSAndDBScan.DataObjects.EuclideanDataObject")
    ),
    properties = "numerics",
    name = "DBScan Clustering",
    short.name = "dbscan",
    note = "You might have to install the Weka package: WPM('install-package', 'optics_dbScan')"
  )
}

#' @export
trainLearner.cluster.DBScan = function(.learner, .task, .subset, .weights = NULL,  ...) {
  ctrl = RWeka::Weka_control(...)
  RWeka::DBScan(getTaskData(.task, .subset), control = ctrl)
}

#' @export
predictLearner.cluster.DBScan = function(.learner, .model, .newdata, ...) {
  # DBScan returns cluster indices (i.e. starting from 0, which some tools don't like
  predict(.model$learner.model, .newdata, ...) + 1
}

