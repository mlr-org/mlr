#' @export
makeRLearner.cluster.MiniBatchKmeans = function() {
  makeRLearnerCluster(
    cl = "cluster.MiniBatchKmeans",
    package = "ClusterR",
    par.set = makeParamSet(
      makeIntegerLearnerParam(id = "clusters", default = 2L, lower = 1L),
      makeIntegerLearnerParam(id = "batch_size", default = 10L, lower = 1L),
      makeIntegerLearnerParam(id = "num_init", default = 1L, lower = 1L),
      makeIntegerLearnerParam(id = "max_iters", default = 100L, lower = 1L),
      makeNumericLearnerParam(id = "init_fraction", default = 1, lower = 0),
      makeDiscreteLearnerParam(id = "initializer", default = "kmeans++",
        values = c("optimal_init", "quantile_init", "kmeans++", "random")),
      makeIntegerLearnerParam(id = "early_stop_iter", default = 10L, lower = 1L),
      makeLogicalLearnerParam(id = "verbose", default = FALSE,
        tunable = FALSE),
      makeUntypedLearnerParam(id = "CENTROIDS", default = NULL),
      makeNumericLearnerParam(id = "tol", default = 1e-04, lower = 0),
      makeNumericLearnerParam(id = "tol_optimal_init", default = 0.3, lower = 0),
      makeIntegerLearnerParam(id = "seed", default = 1L)
    ),
    par.vals = list(clusters = 2L),
    properties = c("numerics", "prob"),
    name = "MiniBatchKmeans",
    note = "Calls MiniBatchKmeans of package ClusterR. Argument `clusters` has default value of 2 if not provided by user.",
    short.name = "MBatchKmeans",
    callees = c("MiniBatchKmeans", "predict_MBatchKMeans")
  )
}

#' @export
trainLearner.cluster.MiniBatchKmeans = function(.learner, .task, .subset, .weights = NULL, ...) {
  ClusterR::MiniBatchKmeans(getTaskData(.task, .subset), ...)
}

#' @export
predictLearner.cluster.MiniBatchKmeans = function(.learner, .model, .newdata, ...) {
  if (.learner$predict.type == "prob") {
    pred = ClusterR::predict_MBatchKMeans(data = .newdata,
      CENTROIDS = .model$learner.model$centroids,
      fuzzy = TRUE, ...)

    res = pred$fuzzy_clusters

    return(res)
  } else {
    pred = ClusterR::predict_MBatchKMeans(data = .newdata,
      CENTROIDS = .model$learner.model$centroids,
      fuzzy = FALSE, ...)

    res = as.integer(pred)

    return(res)
  }
}
