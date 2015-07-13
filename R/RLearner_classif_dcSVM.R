#' @export
makeRLearner.classif.dcSVM = function() {
  makeRLearnerClassif(
    cl = "classif.dcSVM",
    package = "SwarmSVM",
    par.set = makeParamSet(
      makeIntegerLearnerParam(id = "k", default = 4, lower = 1),
      makeIntegerLearnerParam(id = "m", lower = 1),
      makeDiscreteLearnerParam(id = "kernel", default = 3, values = c(1,2,3)),
      makeIntegerLearnerParam(id = "max.levels", lower = 1),
      makeIntegerLearnerParam(id = "early", lower = 0),
      makeLogicalLearnerParam(id = "final.training", default = FALSE),
      makeLogicalLearnerParam(id = "pre.scale", default = FALSE),
      makeNumericLearnerParam(id = "seed", default = NULL),
      makeLogicalLearnerParam(id = "verbose", default = TRUE),
      makeUntypedLearnerParam(id = "valid.x", default = NULL),
      makeUntypedLearnerParam(id = "valid.y", default = NULL),
      makeUntypedLearnerParam(id = "valid.metric", default = NULL),
      makeDiscreteLearnerParam(id = "cluster.method", default = "kmeans", values = c("kmeans", "kernkmeans")),
      makeFunctionLearnerParam(id = "cluster.fun", default = NULL),
      makeFunctionLearnerParam(id = "cluster.predict", default = NULL)
    ),
    properties = c("twoclass"),
    name = "Divided-Conquer Support Vector Machines",
    short.name = "dcSVM",
    note = ""
  )
}

#' @export
trainLearner.classif.dcSVM = function(.learner, .task, .subset, .weights = NULL, ...) {
  d = getTaskData(.task, .subset, target.extra = TRUE)
  SwarmSVM::dcSVM(x = d$data, y = d$target, ...)
}

#' @export
predictLearner.classif.dcSVM = function(.learner, .model, .newdata, ...) {
  as.factor(predict(.model$learner.model, newdata = .newdata, ...))
}
