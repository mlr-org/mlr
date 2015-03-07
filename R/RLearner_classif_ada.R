#' @export
makeRLearner.classif.ada = function() {
  makeRLearnerClassif(
    cl = "classif.ada",
    package = "ada",
    par.set = makeParamSet(
      makeDiscreteLearnerParam(id = "loss", default = "exponential", values = c("exponential", "logistic")),
      makeDiscreteLearnerParam(id = "type", default = "discrete", values = c("discrete", "real", "gentle")),
      makeIntegerLearnerParam(id = "iter", default = 50L, lower = 1L),
      makeNumericLearnerParam(id = "nu", default = 0.1, lower = 0),
      makeNumericLearnerParam(id = "bag.frac", default = 0.5, lower = 0, upper = 1),
      makeLogicalLearnerParam(id = "model.coef", default = TRUE),
      makeLogicalLearnerParam(id = "bag.shift", default = FALSE),
      makeIntegerLearnerParam(id = "max.iter", default = 20L, lower = 1L),
      makeNumericLearnerParam(id = "delta", default = 1e-10, lower = 0),
      makeIntegerLearnerParam(id = "minsplit", default = 20L, lower = 1L),
      makeIntegerLearnerParam(id = "minbucket", lower = 1L),
      makeNumericLearnerParam(id = "cp", default = 0.01, lower = 0, upper = 1),
      makeIntegerLearnerParam(id = "maxcompete", default = 4L, lower = 0L),
      makeIntegerLearnerParam(id = "maxsurrogate", default = 5L, lower = 0L),
      makeDiscreteLearnerParam(id = "usesurrogate", default = 2L, values = 0:2),
      makeDiscreteLearnerParam(id = "surrogatestyle", default = 0L, values = 0:1),
      # we use 30 as upper limit, see docs of rpart.control
      makeIntegerLearnerParam(id = "maxdepth", default = 30L, lower = 1L, upper = 30L)
    ),
    properties = c("twoclass", "numerics", "factors", "prob", "weights"),
    name = "ada Boosting",
    short.name = "ada"
  )
}

#' @export
trainLearner.classif.ada = function(.learner, .task, .subset, .weights = NULL, maxdepth,  ...) {
  f = getTaskFormula(.task)
  ctrl = learnerArgsToControl(rpart::rpart.control, maxdepth)
  ada::ada(f, data = getTaskData(.task, .subset), ...)
}

#' @export
predictLearner.classif.ada = function(.learner, .model, .newdata, ...) {
  type = ifelse(.learner$predict.type=="response", "vector", "prob")
  p = predict(.model$learner.model, newdata = .newdata, type = type, ...)
  if (type == "prob")
    colnames(p) = rownames(.model$learner.model$confusion)
  return(p)
}
