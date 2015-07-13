#' @export
makeRLearner.classif.clusterSVM = function() {
  makeRLearnerClassif(
    cl = "classif.clusterSVM",
    package = c("SwarmSVM", "LiblineaR"),
    par.set = makeParamSet(
      makeIntegerLearnerParam(id = "centers", default = NULL, lower = 1),
      makeUntypedLearnerParam(id = "cluster.object", default = NULL, requires = expression(is.null(centers))),
      makeNumericLearnerParam(id = "lambda", default = 1, lower = 0),
      makeLogicalLearnerParam(id = "sparse", default = TRUE),
      makeUntypedLearnerParam(id = "valid.x", default = NULL),
      makeUntypedLearnerParam(id = "valid.y", default = NULL),
      makeUntypedLearnerParam(id = "valid.metric", default = NULL),
      makeIntegerLearnerParam(id = "type", default = 1, values = c(1, 2, 3, 5)),
      makeNumericLearnerParam(id = "cost", default = 1, lower = 0),
      # better default epsilon is dependent on type ( = NULL, see docs), but we cannot store this
      makeNumericLearnerParam(id = "epsilon", lower = 0),
      makeLogicalLearnerParam(id = "bias", default = TRUE),
      makeNumericVectorLearnerParam(id = "wi", default = NULL, len = NA_integer_),
      makeIntegerLearnerParam(id = "verbose", default = 1, values = c(0, 1, 2)),
      makeNumericLearnerParam(id = "seed", default = NULL),
      makeDiscreteLearnerParam(id = "cluster.method", default = "kmeans", values = c("kmeans", "kernkmeans")),
      makeFunctionLearnerParam(id = "cluster.fun", default = NULL),
      makeFunctionLearnerParam(id = "cluster.predict", default = NULL)
    ),
    properties = c("twoclass"),
    name = "Clustered Support Vector Machines",
    short.name = "clusterSVM",
    note = ""
  )
}

#' @export
trainLearner.classif.clusterSVM = function(.learner, .task, .subset, .weights = NULL, ...) {
  d = getTaskData(.task, .subset, target.extra = TRUE)
  SwarmSVM::clusterSVM(x = d$data, y = d$target, ...)
}

#' @export
predictLearner.classif.clusterSVM = function(.learner, .model, .newdata, ...) {
  as.factor(predict(.model$learner.model, newdata = .newdata, ...))
}
