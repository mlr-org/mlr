#' @export
makeRLearner.cluster.dbscan = function() {
  makeRLearnerCluster(
    cl = "cluster.dbscan",
    package = "fpc",
    par.set = makeParamSet(
      makeNumericLearnerParam(id = "eps", default = 1, lower = 0),
      # FIXME eps seems to have no default in dbscan(), if it has 1 par.vals is redundant
      makeIntegerLearnerParam(id = "MinPts", default = 5L, lower = 0L),
      makeLogicalLearnerParam(id = "scale", default = FALSE),
      makeLogicalLearnerParam(id = "showplot", default = FALSE, tunable = FALSE),
      makeDiscreteLearnerParam(id = "method", values = c("hybrid", "raw", "dist"), default = "hybrid")
    ),
    par.vals = list(eps = 1),
    properties = "numerics",
    name = "DBScan Clustering",
    note = "A cluster index of NA indicates noise points. Specify `method = 'dist'` if the data should be interpreted as dissimilarity matrix or object. Otherwise Euclidean distances will be used.",
    short.name = "dbscan",
    callees = "dbscan"
  )
}

#' @export
trainLearner.cluster.dbscan = function(.learner, .task, .subset, .weights = NULL, ...) {
  data = getTaskData(.task, .subset)
  model = fpc::dbscan(data, ...)
  # dbscan needs this in the prediction phase
  model$data = data
  return(model)
}

#' @export
predictLearner.cluster.dbscan = function(.learner, .model, .newdata, ...) {
  indices = as.integer(predict(.model$learner.model, .model$learner.model$data, newdata = .newdata, ...))
  indices[indices == 0L] = NA_integer_
  return(indices)
}
