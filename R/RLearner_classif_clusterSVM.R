#' @export
makeRLearner.classif.clusterSVM = function() {
  makeRLearnerClassif(
    cl = "classif.clusterSVM",
    package = c("SwarmSVM", "LiblineaR"),
    par.set = makeParamSet(
      makeIntegerLearnerParam(id = "centers", default = 2, lower = 1),
      # FIXME default for centers of clusterSVM is NULL
      makeUntypedLearnerParam(id = "cluster.object", default = NULL, requires = quote(is.null(centers))),
      makeNumericLearnerParam(id = "lambda", default = 1, lower = 0),
      makeLogicalLearnerParam(id = "sparse", default = TRUE),
      makeUntypedLearnerParam(id = "valid.x", default = NULL),
      makeUntypedLearnerParam(id = "valid.y", default = NULL),
      makeUntypedLearnerParam(id = "valid.metric", default = NULL),
      makeDiscreteLearnerParam(id = "type", default = 1, values = c(1, 2, 3, 5)),
      makeNumericLearnerParam(id = "cost", default = 1, lower = 0),
      # better default epsilon is dependent on type ( = NULL, see docs), but we cannot store this
      makeNumericLearnerParam(id = "epsilon", lower = 0),
      makeLogicalLearnerParam(id = "bias", default = TRUE),
      makeNumericVectorLearnerParam(id = "wi", len = NA_integer_),
      makeDiscreteLearnerParam(id = "verbose", default = 1, values = c(0, 1, 2)),
      makeNumericLearnerParam(id = "seed"),
      makeDiscreteLearnerParam(id = "cluster.method", default = "kmeans", values = c("kmeans", "kernkmeans")),
      makeFunctionLearnerParam(id = "cluster.fun"),
      makeFunctionLearnerParam(id = "cluster.predict")
    ),
    par.vals = list(centers = 2),
    properties = c("twoclass", "numerics"),
    name = "Clustered Support Vector Machines",
    short.name = "clusterSVM",
    note = "`centers` set to `2` by default.",
    callees = "clusterSVM"
  )
}

#' @export
trainLearner.classif.clusterSVM = function(.learner, .task, .subset, .weights = NULL, ...) {
  d = getTaskData(.task, .subset, target.extra = TRUE)
  SwarmSVM::clusterSVM(x = d$data, y = d$target, ...)
}

#' @export
predictLearner.classif.clusterSVM = function(.learner, .model, .newdata, ...) {
  as.factor(predict(.model$learner.model, newdata = .newdata, ...)$predictions)
}
