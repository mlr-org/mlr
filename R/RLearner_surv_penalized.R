#' @export
makeRLearner.surv.penalized = function() {
  makeRLearnerSurv(
    cl = "surv.penalized",
    package = "penalized",
    par.set = makeParamSet(
      makeNumericLearnerParam(id="lambda1", default=0, lower=0),
      makeNumericLearnerParam(id="lambda2", default=0, lower=0),
      makeLogicalLearnerParam(id="fusedl", default=FALSE),
      makeLogicalLearnerParam(id="standardize", default=FALSE),
      makeIntegerLearnerParam(id="maxiter", default=25L)
    ),
    # FIXME this depends on the prediction method ...
    missings = FALSE,
    numerics = TRUE,
    factors = FALSE,
    se = FALSE,
    weights = FALSE
  )
}

#' @export
trainLearner.surv.penalized = function(.learner, .task, .subset, .weights,  ...) {
  f = getTaskFormula(.task, env = as.environment("package:survival"))
  penalized(f, data=getTaskData(.task, .subset), model = "cox", trace = FALSE, ...)
}

#' @export
predictLearner.surv.penalized = function(.learner, .model, .newdata, ...) {
  model = .learner$learner.model
  .newdata = model.matrix(model@formula$penalized, .newdata)
}
