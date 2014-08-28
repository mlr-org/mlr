#' @export
makeRLearner.cluster.kmeans = function() {
  makeRLearnerCluster(
    cl = "cluster.kmeans",
    package = c("stats", "clue"),
    par.set = makeParamSet(
      makeUntypedLearnerParam(id = "centers"),
      makeIntegerLearnerParam(id = "iter.max", default = 10L, lower = 1L),
      makeIntegerLearnerParam(id = "nstart", default = 1L, lower = 1L),
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

