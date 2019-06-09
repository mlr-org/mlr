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
        values = c("Hartigan-Wong", "Lloyd", "Forgy", "MacQueen"), default = "Hartigan-Wong"),
      makeLogicalLearnerParam(id = "trace", tunable = FALSE)
    ),
    par.vals = list(centers = 2L),
    properties = c("numerics", "prob"),
    name = "K-Means",
    note = "The `predict` method uses `cl_predict` from the `clue` package to compute the cluster memberships for new data. The default `centers = 2` is added so the method runs without setting parameters, but this must in reality of course be changed by the user.",
    short.name = "kmeans",
    callees = c("kmeans", "cl_predict")
  )
}

#' @export
trainLearner.cluster.kmeans = function(.learner, .task, .subset, .weights = NULL, ...) {
  stats::kmeans(getTaskData(.task, .subset), ...)
}

#' @export
predictLearner.cluster.kmeans = function(.learner, .model, .newdata, ...) {
  switch(.learner$predict.type,
    response = as.integer(clue::cl_predict(.model$learner.model, newdata = .newdata, type = "class_ids", ...)),
    prob = as.matrix(clue::cl_predict(.model$learner.model, newdata = .newdata, type = "memberships", ...))
  )
}
