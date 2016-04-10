#' @export
makeRLearner.classif.penalized.ridge = function() {
  makeRLearnerClassif(
    cl = "classif.penalized.ridge",
    package = "!penalized",
    par.set = makeParamSet(
      makeNumericLearnerParam(id = "lambda2", default = 0, lower = 0),
      makeUntypedLearnerParam(id = "unpenalized"),
      makeLogicalVectorLearnerParam(id = "positive", default = FALSE),
      makeNumericVectorLearnerParam(id = "startbeta"),
      makeNumericVectorLearnerParam(id = "startgamma"),
      makeNumericLearnerParam(id = "epsilon", lower = 0, default = 1e-10),
      # FIXME: Parameter dependent default for maxiter:
      # default is 25 if lambda1 = 0, lambda2 > 0, infinite otherwise
      makeIntegerLearnerParam(id = "maxiter", lower = 1L),
      makeLogicalLearnerParam(id = "standardize", default = FALSE),
      makeLogicalLearnerParam(id = "trace", default = FALSE, tunable = FALSE)
    ),
    par.vals = list(trace = FALSE),
    properties = c("twoclass", "numerics", "factors", "ordered", "prob"),
    name = "Logistic Ridge Regression",
    short.name = "ridge",
    note = "trace=FALSE was set by default to disable logging output."
  )
}

#' @export
trainLearner.classif.penalized.ridge = function(.learner, .task, .subset, .weights = NULL,  ...) {
  f = getTaskFormula(.task)
  penalized::penalized(f, data = getTaskData(.task, .subset), fusedl = FALSE, model = "logistic", ...)
}

#' @export
predictLearner.classif.penalized.ridge = function(.learner, .model, .newdata, ...) {
  m = .model$learner.model
  levs = .model$task.desc$class.levels
  .newdata[,.model$task.desc$target] = 0
  pred = penalized::predict(m, data = .newdata,  ...)
  if (.learner$predict.type == "prob") {
    propVectorToMatrix(pred, levs)
  } else {
    as.factor(ifelse(pred > 0.5, levs[2L], levs[1L]))
  }
}
