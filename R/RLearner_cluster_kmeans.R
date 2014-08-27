#' @export
makeRLearner.cluster.kmeans = function() {
  makeRLearnerCluster(
    cl = "cluster.kmeans",
    package = c("stats", "clue"),
    par.set = makeParamSet(
      makeUntypedLearnerParam(id = "centers"),
      makeNumericLearnerParam(id = "iter.max", default = 10, lower = 1),
      makeNumericLearnerParam(id = "nstart", default = 1, lower = 1),
      makeDiscreteLearnerParam(id = "algorithm",
                               values = c("Hartigan-Wong", "Lloyd", "Forgy",
                                          "MacQueen"),
                               default = 'Hartigan-Wong'),
      makeLogicalLearnerParam(id = "trace")
    ),
    properties = c("numerics"),
    name = "k-means",
    note = "The 'predict' method uses 'cl_predict' from the 'clue' package to compute the cluster memberships for new data.",
    short.name = "kmeans"
  )
}

#' @export
trainLearner.cluster.kmeans = function(.learner, .task, .subset, .weights = NULL, ...) {
  kmeans(getTaskData(.task, .subset), ...)
}

#' @export
predictLearner.cluster.kmeans = function(.learner, .model, .newdata, ...) {
  as.integer(cl_predict(.model$learner.model, newdata = .newdata, type = "class_ids"))
}

