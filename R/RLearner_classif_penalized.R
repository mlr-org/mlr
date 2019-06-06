#' @export
makeRLearner.classif.penalized = function() {
  makeRLearnerClassif(
    cl = "classif.penalized",
    package = "!penalized",
    par.set = makeParamSet(
      makeNumericLearnerParam(id = "lambda1", default = 0, lower = 0),
      makeNumericLearnerParam(id = "lambda2", default = 0, lower = 0),
      makeLogicalLearnerParam(id = "fusedl", default = FALSE),
      makeUntypedLearnerParam(id = "unpenalized", tunable = FALSE),
      makeLogicalVectorLearnerParam(id = "positive", default = FALSE),
      makeNumericVectorLearnerParam(id = "startbeta"),
      makeNumericVectorLearnerParam(id = "startgamma"),
      # untyped here because one can also pass "Park" to steps
      makeUntypedLearnerParam(id = "steps", default = 1L, tunable = FALSE),
      makeNumericLearnerParam(id = "epsilon", lower = 0, default = 1e-10),
      makeIntegerLearnerParam(id = "maxiter", lower = 1L),
      makeLogicalLearnerParam(id = "standardize", default = FALSE),
      makeLogicalLearnerParam(id = "trace", default = TRUE, tunable = FALSE)
    ),
    par.vals = list(trace = FALSE),
    properties = c("twoclass", "numerics", "factors", "ordered", "prob"),
    name = "Penalized Logistic Regression",
    short.name = "penalized",
    note = "trace=FALSE was set by default to disable logging output.",
    callees = "penalized"
  )
}

#' @export
trainLearner.classif.penalized = function(.learner, .task, .subset, .weights = NULL, ...) {
  f = getTaskFormula(.task)
  penalized::penalized(f, data = getTaskData(.task, .subset), model = "logistic", ...)
}

#' @export
predictLearner.classif.penalized = function(.learner, .model, .newdata, ...) {
  m = .model$learner.model
  levs = .model$task.desc$class.levels
  # FIXME: should be removed, reported in issue 840
  m@formula$unpenalized[[2L]] = as.symbol(.model$task.desc$target)
  .newdata[, .model$task.desc$target] = 0
  pred = penalized::predict(m, data = .newdata, ...)
  if (.learner$predict.type == "prob") {
    propVectorToMatrix(pred, levs)
  } else {
    as.factor(ifelse(pred > 0.5, levs[2L], levs[1L]))
  }
}
