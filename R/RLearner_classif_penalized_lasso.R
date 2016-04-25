#' @export
makeRLearner.classif.penalized.lasso = function() {
  makeRLearnerClassif(
    cl = "classif.penalized.lasso",
    package = "!penalized",
    par.set = makeParamSet(
      makeNumericLearnerParam(id = "lambda1", default = 0, lower = 0),
      makeUntypedLearnerParam(id = "unpenalized"),
      makeLogicalVectorLearnerParam(id = "positive", default = FALSE),
      makeNumericVectorLearnerParam(id = "startbeta"),
      makeNumericVectorLearnerParam(id = "startgamma"),
      # untyped here because one can also pass "Park" to steps
      makeUntypedLearnerParam(id = "steps", default = 1L),
      makeNumericLearnerParam(id = "epsilon", lower = 0, default = 1e-10),
      makeIntegerLearnerParam(id = "maxiter", lower = 1L),
      makeLogicalLearnerParam(id = "standardize", default = FALSE),
      makeLogicalLearnerParam(id = "trace", default = FALSE, tunable = FALSE)
    ),
    par.vals = list(trace = FALSE),
    properties = c("twoclass", "numerics", "factors", "ordered", "prob"),
    name = "Logistic Lasso Regression",
    short.name = "lasso",
    note = "trace=FALSE was set by default to disable logging output."
  )
}

#' @export
trainLearner.classif.penalized.lasso = function(.learner, .task, .subset, .weights = NULL,  ...) {
  f = getTaskFormula(.task)
  penalized::penalized(f, data = getTaskData(.task, .subset), fusedl = FALSE, model = "logistic", ...)
}

#' @export
predictLearner.classif.penalized.lasso = function(.learner, .model, .newdata, ...) {
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
