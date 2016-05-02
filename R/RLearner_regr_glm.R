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
        values = c("1/mu^2", "inverse", "identity", "log"), requires = quote (family == "inverse.gaussian")),
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
      family = "gaussian"
    ),
    properties = c("numerics", "factors", "se", "weights"),
    name = "Generalized Linear Regression",
    note = "Mlr derivates from original glm API: the learner parameter family 
    must be a character and every family has its own link, i.e. 
    family = 'gaussian', link.gaussian = 'identity'",
    short.name = "glm"
  )
}

#' @export
trainLearner.regr.glm = function(.learner, .task, .subset, .weights = NULL, epsilon, maxit, trace, family, ...) {
  ctrl = learnerArgsToControl(stats::glm.control, epsilon, maxit, trace)
  d = getTaskData(.task, .subset)
  f = getTaskFormula(.task)
  family = .learner$par.vals$family
  
  if (family == "gaussian") {
    if (length(.learner$par.vals$gaussian.link) == 0L) { 
      family = stats::gaussian()
    } else {
      family = stats::gaussian(link = .learner$par.vals$gaussian.link)
    }
  } else {
    if (family == "poisson") {
      if (length(.learner$par.vals$poisson.link) == 0L) {
        family = stats::poisson()
      } else {
        family = stats::poisson(link = .learner$par.vals$poisson.link)
      }
    } else {
      if (family == "Gamma") {
        if (length(.learner$par.vals$Gamma.link) == 0L) {
          family = stats::Gamma()
        } else {
          family = stats::Gamma(link = .learner$par.vals$Gamma.link)
        }
      } else {
        if (family == "inverse.gaussian") {
          if (length(.learner$par.vals$Gamma.link) == 0L) {
            family = stats::inverse.gaussian()
          } else {
            family = stats::inverse.gaussian(link = .learner$par.vals$inverse.gaussian.link)
          }
        }
      }
    }
  }
  
  
  if (is.null(.weights)) {
    stats::glm(f, data = d, control = ctrl, family = family, ... )
  } else  {
    stats::glm(f, data = d, control = ctrl, weights = .weights, family = family, ... )
  }
}

#' @export
predictLearner.regr.glm = function(.learner, .model, .newdata, ...) {
  if (.learner$predict.type == "response") {
    predict(.model$learner.model, newdata = .newdata, se.fit = FALSE, ...)
  } else {
    p = predict(.model$learner.model, newdata = .newdata, se.fit = TRUE, ...)
    cbind(p$fit, p$se.fit)
  }
}
