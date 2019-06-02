#' @export
makeRLearner.regr.km = function() {
  makeRLearnerRegr(
    cl = "regr.km",
    package = "DiceKriging",
    par.set = makeParamSet(
      makeDiscreteLearnerParam(id = "covtype", default = "matern5_2",
        values = list("gauss", "matern5_2", "matern3_2", "exp", "powexp")),
      makeNumericVectorLearnerParam(id = "coef.trend"),
      makeNumericVectorLearnerParam(id = "coef.cov"),
      makeNumericVectorLearnerParam(id = "coef.var"),
      makeNumericLearnerParam(id = "nugget"),
      makeLogicalLearnerParam(id = "nugget.estim", default = FALSE),
      makeNumericVectorLearnerParam(id = "noise.var"),
      makeDiscreteLearnerParam(id = "estim.method", default = "MLE",
        values = c("MLE", "LOO")),
      makeDiscreteLearnerParam(id = "optim.method", default = "BFGS",
        values = c("BFGS", "gen")),
      makeNumericVectorLearnerParam(id = "lower"),
      makeNumericVectorLearnerParam(id = "upper"),
      makeNumericVectorLearnerParam(id = "parinit"),
      makeIntegerLearnerParam(id = "multistart", default = 1L, lower = 1L),
      makeUntypedLearnerParam(id = "control"),
      makeLogicalLearnerParam(id = "gr", default = TRUE),
      makeLogicalLearnerParam(id = "iso", default = FALSE),
      makeLogicalLearnerParam(id = "scaling", default = FALSE),
      makeUntypedLearnerParam(id = "knots"),
      makeLogicalLearnerParam(id = "jitter", default = FALSE, when = "predict"),
      makeNumericLearnerParam(id = "nugget.stability", requires = quote(!nugget.estim && is.null(nugget)))
    ),
    par.vals = list(jitter = FALSE),
    # FIXME jitter not found as parameter for km() or km.predict(). par.vals and LearnerParam are the same here.
    properties = c("numerics", "se"),
    name = "Kriging",
    short.name = "km",
    note = 'In predict, we currently always use `type = "SK"`. The extra parameter `jitter` (default is `FALSE`) enables adding a very small jitter (order 1e-12) to the x-values before prediction, as `predict.km` reproduces the exact y-values of the training data points, when you pass them in, even if the nugget effect is turned on. \n We further introduced `nugget.stability` which sets the `nugget` to `nugget.stability * var(y)` before each training to improve numerical stability. We recommend a setting of 10^-8',
    callees = "km"
  )
}

#' @export
trainLearner.regr.km = function(.learner, .task, .subset, .weights = NULL, ...) {
  d = getTaskData(.task, .subset, target.extra = TRUE)
  args = list(...)
  if (!is.null(args$optim.method) && args$optim.method == "gen") {
    requirePackages(packs = "rgenoud", why = "fitting 'regr.km' with 'rgenoud' optimization")
  }
  if (!is.null(args$nugget.stability)) {
    if (args$nugget.stability == 0) {
      args$nugget = 0
    } else {
      args$nugget = args$nugget.stability * var(d$target)
    }
    args$nugget.stability = NULL
  }
  do.call(DiceKriging::km, c(list(design = d$data, response = d$target), args))
}

#' @export
predictLearner.regr.km = function(.learner, .model, .newdata, jitter, ...) {
  # km with nugget estim perfectly interpolate the datas ONLY at exactly the training points
  # see JSS paper for explanation
  # so we add minimal, numerical jitter to the x points
  if (jitter) {
    jit = matrix(rnorm(nrow(.newdata) * ncol(.newdata), mean = 0, sd = 1e-12), nrow = nrow(.newdata))
    .newdata = .newdata + jit
  }
  se = (.learner$predict.type != "response")
  p = DiceKriging::predict.km(.model$learner.model, newdata = .newdata, type = "SK", se.compute = se)
  if (!se) {
    return(p$mean)
  } else {
    cbind(p$mean, p$sd)
  }
}
