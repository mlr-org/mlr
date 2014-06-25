#' @export
makeRLearner.surv.penalized = function() {
  makeRLearnerSurv(
    cl = "surv.penalized",
    package = "penalized",
    par.set = makeParamSet(
      makeNumericLearnerParam(id = "lambda1", default = 0, lower = 0),
      makeNumericLearnerParam(id = "lambda2", default = 0, lower = 0),
      makeLogicalLearnerParam(id = "fusedl", default = FALSE),
      makeLogicalLearnerParam(id = "standardize", default = FALSE),
      makeIntegerLearnerParam(id = "maxiter", default = 25L)
    ),
    properties = c("numerics", "rcens")
  )
}

#' @export
trainLearner.surv.penalized = function(.learner, .task, .subset, .weights = NULL,  ...) {
  f = getTaskFormula(.task, env = as.environment("package:survival"))
  penalized(f, data = getTaskData(.task, .subset), model = "cox", trace = FALSE, ...)
}

#' @export
predictLearner.surv.penalized = function(.learner, .model, .newdata, ...) {
  model = .model$learner.model
  # FIXME: add possibility to handle factors
  # .newdata = addContrasts(.newdata)
  predict(model, penalized = model.matrix(model@formula$penalized, .newdata)[, -1])
}

addContrasts = function(data) {
  fac.inds = which(sapply(data, is.factor))
  for (i in fac.inds) {
    n = nlevels(data[, i])
    contr = contr.none(n)
    if (n > 2) {
      colnames(contr) = levels(data[, i])
      contrasts(data[, i], how.many = n) = contr
    } else if (n == 2) {
      colnames(contr) = levels(data[, i])[2]
      contrasts(data[, i], how.many = 1) = contr
    }
  }
  return(data)
}
