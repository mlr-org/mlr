#' @export
makeRLearner.classif.penalized.fusedlasso = function() {
  makeRLearnerClassif(
    cl = "classif.penalized.fusedlasso",
    package = "!penalized",
    par.set = makeParamSet(
      makeNumericLearnerParam(id = "lambda1", default = 1, lower = 0),
      makeNumericLearnerParam(id = "lambda2", default = 1, lower = 0),
      makeUntypedLearnerParam(id = "unpenalized"),
      makeLogicalVectorLearnerParam(id = "positive", default = FALSE),
      makeDiscreteLearnerParam(id = "model", default = "linear",
        values = c("linear", "poisson")),
      makeNumericVectorLearnerParam(id = "startbeta"),
      makeNumericVectorLearnerParam(id = "startgamma"),
      # untyped here because one can also pass "Park" to steps
      makeUntypedLearnerParam(id = "steps", default = 1L),
      makeNumericLearnerParam(id = "epsilon", lower = 0, default = 1e-10),
      makeIntegerLearnerParam(id = "maxiter", lower = 1L),
      makeLogicalLearnerParam(id = "standardize", default = FALSE),
      makeLogicalLearnerParam(id = "trace", default = FALSE, tunable = FALSE)
    ),
    mlr.defaults = list(trace = FALSE, lambda1 = 1, lambda2 = 1),
    properties = c("twoclass", "numerics", "factors", "prob"),
    name = "Logistic Fused Lasso Regression",
    short.name = "fusedlasso",
    note = "trace=FALSE was set by default to disable logging output. lambda1 and lambda2 have been set to 1 by default, as fusedlasso needs both penalizations > 0."
  )
}

#' @export
trainLearner.classif.penalized.fusedlasso = function(.learner, .task, .subset, .weights = NULL,  ...) {
  f = getTaskFormula(.task)
  penalized::penalized(f, data = getTaskData(.task, .subset), fusedl = TRUE, model = "logistic", ...)
}

#' @export
predictLearner.classif.penalized.fusedlasso = function(.learner, .model, .newdata, ...) {
  m = .model$learner.model
  levs = .model$task.desc$class.levels
  # FIXME: should be removed, reported in issue 840
  m@formula$unpenalized[[2L]] = as.symbol(.model$task.desc$target)
  .newdata[,.model$task.desc$target] = 0
  pred = penalized::predict(m, data = .newdata,  ...)
  if (.learner$predict.type == "prob") {
    propVectorToMatrix(pred, levs)
  } else {
    as.factor(ifelse(pred > 0.5, levs[2L], levs[1L]))
  }
}
