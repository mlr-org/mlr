#' @export
makeRLearner.classif.multinom = function() {
  makeRLearnerClassif(
    cl = "classif.multinom",
    package = "nnet",
    par.set = makeParamSet(
      makeLogicalLearnerParam(id = "Hess", default = FALSE, tunable = FALSE),
      makeDiscreteLearnerParam(id = "summ", default = 0L, values = 0:3),
      makeLogicalLearnerParam(id = "censored", default = FALSE),
      makeLogicalLearnerParam(id = "model", default = FALSE, tunable = FALSE),
      makeIntegerLearnerParam(id = "maxit", default = 100L, lower = 1L),
      makeNumericLearnerParam(id = "rang", default = 0.7),
      makeNumericLearnerParam(id = "decay", default = 0),
      makeLogicalLearnerParam(id = "trace", default = TRUE, tunable = FALSE),
      makeNumericLearnerParam(id = "abstol", default = 1.0e-4),
      makeNumericLearnerParam(id = "reltol", default = 1.0e-8)
    ),
    properties = c("twoclass", "multiclass", "numerics", "factors", "prob", "weights"),
    name = "Multinomial Regression",
    short.name = "multinom",
    callees = c("multinom", "nnet")
  )
}

#' @export
trainLearner.classif.multinom = function(.learner, .task, .subset, .weights = NULL, ...) {
  if (is.null(.weights)) {
    f = getTaskFormula(.task)
    nnet::multinom(f, data = getTaskData(.task, .subset), ...)
  } else {
    f = getTaskFormula(.task)
    nnet::multinom(f, data = getTaskData(.task, .subset), weights = .weights, ...)
  }
}

#' @export
predictLearner.classif.multinom = function(.learner, .model, .newdata, ...) {
  type = ifelse(.learner$predict.type == "response", "class", "probs")
  levs = .model$task.desc$class.levels
  p = predict(.model$learner.model, newdata = .newdata, type = type, ...)
  if (type == "probs" && length(levs) == 2L) {
    p = matrix(c(1 - p, p), ncol = 2L, byrow = FALSE)
    colnames(p) = levs
  }
  return(p)
}
