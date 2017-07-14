#' @export
makeRLearner.regr.glm = function() {
  makeRLearnerRegr(
    cl = "regr.glm",
    package = "stats",
    par.set = makeParamSet(
      makeDiscreteLearnerParam(id = "family", default = "gaussian",
        values = c("gaussian", "Gamma", "inverse.gaussian", "poisson")),
      makeDiscreteLearnerParam(id = "gaussian.link", default = "identity",
        values = c("identity", "log", "inverse"), requires = quote(family == "gaussian")),
      makeDiscreteLearnerParam(id = "Gamma.link", default = "inverse",
        values = c("inverse", "identity", "log"), requires = quote(family == "Gamma")),
      makeDiscreteLearnerParam(id = "poisson.link", default = "log",
        values = c("log", "identity", "sqrt"), requires = quote(family == "poisson")),
      makeDiscreteLearnerParam(id = "inverse.gaussian.link", default = "1/mu^2",
        values = c("1/mu^2", "inverse", "identity", "log"), requires = quote(family == "inverse.gaussian")),
      # FIXME: default for start, mustart and etastart is family and link dependet (see family$initialize)
      # FIXME: length is data dependent (length = number of predictors + 1)
      makeNumericVectorLearnerParam(id = "start"),
      # FIXME: len for etastart and mustart is data dependet (length = number of cases)
      makeNumericVectorLearnerParam(id = "etastart"),
      makeNumericVectorLearnerParam(id = "mustart"),
      makeNumericVectorLearnerParam(id = "offset"),
      makeNumericLearnerParam(id = "epsilon", default = 1e-8),
      makeIntegerLearnerParam(id = "maxit", default = 25L),
      makeLogicalLearnerParam(id = "trace", default = FALSE, tunable = FALSE),
      makeLogicalLearnerParam(id = "model", default = TRUE, tunable = FALSE),
      makeUntypedLearnerParam(id = "method", default = "glm.fit", tunable = FALSE),
      makeLogicalLearnerParam(id = "x", default = FALSE, tunable = FALSE),
      makeLogicalLearnerParam(id = "y", default = TRUE, tunable = FALSE)
    ),
    par.vals = list(
      family = "gaussian",
      model = FALSE
    ),
    properties = c("numerics", "factors", "se", "weights"),
    name = "Generalized Linear Regression",
    short.name = "glm",
    note = "'family' must be a character and every family has its own link, i.e. family = 'gaussian', link.gaussian = 'identity', which is also the default. We set 'model' to FALSE by default to save memory.",
    callees = c("glm", "glm.control", "gaussian", "poisson", "Gamma", "inverse.gaussian")
  )
}

#' @export
trainLearner.regr.glm = function(.learner, .task, .subset, .weights = NULL, epsilon, maxit, trace, family,
  gaussian.link = "identity", poisson.link = "log", Gamma.link = "inverse", inverse.gaussian.link = "1/mu2", ...) {

  ctrl = learnerArgsToControl(stats::glm.control, epsilon, maxit, trace)
  d = getTaskData(.task, .subset)
  f = getTaskFormula(.task)

  family = switch(family,
    gaussian = stats::gaussian(link = make.link(gaussian.link)),
    poisson = stats::poisson(link = make.link(poisson.link)),
    Gamma = stats::Gamma(link = make.link(Gamma.link)),
    inverse.gaussian = stats::inverse.gaussian(link = make.link(inverse.gaussian.link))
  )
  if (is.null(.weights))
    m = stats::glm(f, data = d, control = ctrl, family = family, ...)
  else
    m = stats::glm(f, data = d, control = ctrl, weights = .weights, family = family, ...)
  return(m)
}

#' @export
predictLearner.regr.glm = function(.learner, .model, .newdata, ...) {
  se.fit = .learner$predict.type == "se"
  p = predict(.model$learner.model, newdata = .newdata, type = "response", se.fit = se.fit, ...)
  if (se.fit)
    p = cbind(p$fit, p$se.fit)
  return(p)
}
