#' @export
makeRLearner.surv.penalized.fusedlasso = function() {
  makeRLearnerSurv(
    cl = "surv.penalized.fusedlasso",
    package = "!penalized",
    par.set = makeParamSet(
      makeNumericLearnerParam(id = "lambda1", default = 1, lower = 0),
      makeNumericLearnerParam(id = "lambda2", default = 1, lower = 0),
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
    par.vals = list(lambda1 = 1, lambda2 = 1, trace = FALSE),
    properties = c("numerics", "factors", "ordered", "rcens"),
    name = "Fused Lasso Regression",
    short.name = "fusedlasso",
    note = "trace=FALSE was set by default to disable logging output. lambda1 and lambda2 have been set to 1 by default, as fusedlasso needs both penalizations > 0."
  )
}

#' @export
trainLearner.surv.penalized.fusedlasso = function(.learner, .task, .subset, .weights = NULL,  ...) {
  data = getTaskData(.task, subset = .subset, target.extra = TRUE, recode.target = "rcens")
  info = getFixDataInfo(data$data, factors.to.dummies = TRUE, ordered.to.int = TRUE)
  data$data = as.matrix(fixDataForLearner(data$data, info))
  attachTrainingInfo(
    penalized::penalized(response = data$target, penalized = data$data, model = "cox",
      fusedl = TRUE, ...),
    info
  )
}

#' @export
predictLearner.surv.penalized.fusedlasso = function(.learner, .model, .newdata, ...) {
  info = getTrainingInfo(.model)
  .newdata = as.matrix(fixDataForLearner(.newdata, info))
  if(.learner$predict.type == "response") {
    # Note: this is a rather ugly hack but should work according to Jelle
    penalized::survival(penalized::predict(.model$learner.model,
      penalized = .newdata), Inf)
  } else {
    stop("Unknown predict type")
  }
}
