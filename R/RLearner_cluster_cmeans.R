#' @export
makeRLearner.cluster.cmeans = function() {
  makeRLearnerCluster(
    cl = "cluster.cmeans",
    package = c("e1071", "clue"),
    par.set = makeParamSet(
      makeUntypedLearnerParam(id = "centers"),
      makeIntegerLearnerParam(id = "iter.max", default = 100L, lower = 1L),
      makeIntegerLearnerParam(id = "m", default = 2L, lower = 1L),
      makeDiscreteLearnerParam(id = "dist", values = c("euclidean", "manhattan"), default = "euclidean"),
      makeUntypedLearnerParam(id = "control"),
      makeLogicalLearnerParam(id = "verbose", default = FALSE, tunable = FALSE),
      makeDiscreteLearnerParam(id = "method", default = "cmeans", values = c("cmeans", "ufcl")),
      makeNumericLearnerParam(id = "rate.par", default = NULL, lower = 0, upper = 1, special.vals = list(NULL)),
      makeNumericLearnerParam(id = "reltol", default = NULL, special.vals = list(NULL))
    ),
    par.vals = list(centers = 2L),
    properties = c("numerics", "prob"),
    name = "Fuzzy C-Means Clustering",
    note = "The `predict` method uses `cl_predict` from the `clue` package to compute the cluster memberships for new data. The default `centers = 2` is added so the method runs without setting parameters, but this must in reality of course be changed by the user.",
    short.name = "cmeans",
    callees = c("cmeans", "cl_predict")
  )
}

#' @export
trainLearner.cluster.cmeans = function(.learner, .task, .subset, .weights = NULL, reltol, ...) {
  ctrl = learnerArgsToControl(list, reltol)
  e1071::cmeans(getTaskData(.task, .subset), control = ctrl, ...)
}

#' @export
predictLearner.cluster.cmeans = function(.learner, .model, .newdata, ...) {
  switch(.learner$predict.type,
    response = as.integer(clue::cl_predict(.model$learner.model, newdata = .newdata, type = "class_ids", ...)),
    prob = as.matrix(clue::cl_predict(.model$learner.model, newdata = .newdata, type = "memberships", ...))
  )
}
