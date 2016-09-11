#'@export
makeRLearner.fcregr.Arima = function() {
  makeRLearnerForecastRegr(
    cl = "fcregr.Arima",
    package = "forecast",
    par.set = makeParamSet(
      makeIntegerVectorLearnerParam(id = "order", len = 3L, lower = 0L, upper = Inf, default = c(0L, 0L, 0L)),
      makeIntegerVectorLearnerParam(id = "seasonal", len = 3L, lower = 0L, upper = Inf, default = c(0L, 0L, 0L)),
      makeLogicalLearnerParam(id = "include.mean", default = TRUE),
      makeLogicalLearnerParam(id = "include.drift", default = FALSE),
      makeNumericLearnerParam(id = "lambda", default = NULL, special.vals = list(NULL), when = "both"),
      makeLogicalLearnerParam(id = "biasadj", default = FALSE, when = "both"),
      makeDiscreteLearnerParam(id = "method", values = c("CSS-ML", "ML", "CSS"), default = "CSS-ML"),
      makeUntypedLearnerParam(id = "model", default = NULL),
      # arima params
      makeLogicalLearnerParam(id = "transform.pars", default = TRUE),
      makeNumericVectorLearnerParam(id = "fixed", len = NA, default = NULL, special.vals = list(NULL)),
      makeNumericVectorLearnerParam(id = "init", len = NA, default = NULL, special.vals = list(NULL)),
      # No default
      makeIntegerLearnerParam("n.cond", lower = 0L),
      makeDiscreteLearnerParam("SSinit", values = c("Gardner1980", "Rossignol2011"), default = "Gardner1980", tunable = FALSE),
      makeDiscreteLearnerParam("optim.method", default = "BFGS", values = c("Nelder-Mead", "BFGS", "CG", "L-BFGS-B", "SANN", "Brent"), tunable = TRUE),
      makeUntypedLearnerParam("optim.controls", default = list(), tunable = FALSE),
      makeNumericLearnerParam("kappa", lower = 1e6, upper = Inf, tunable = FALSE),
      # prediction params
      makeIntegerLearnerParam(id = "h", lower = 0L, upper = Inf,
        # NOTE: object$arma[5] is the frequency of the data
        default = expression(ifelse(object$arma[5] > 1L, 2L * object$arma[5], 10L)),
        tunable = TRUE,
        when = "predict"),
      makeLogicalLearnerParam(id = "bootstrap", default = FALSE, when = "predict", tunable = FALSE),
      makeNumericVectorLearnerParam(id = "level", len = NA, default = c(80, 95), when = "predict", tunable = FALSE),
      makeLogicalLearnerParam(id = "fan", default = FALSE, when = "predict", tunable = FALSE),
      makeIntegerLearnerParam(id = "npaths", default = 5000L, when = "predict"),
      # simulate params
      makeIntegerLearnerParam(id = "nsim", lower = 0L, default = expression(length(object$x)), when = "predict"),
      makeIntegerLearnerParam(id = "seed", default = NULL, special.vals = list(NULL), tunable = FALSE, when = "predict"),
      makeLogicalLearnerParam(id = "future", default = TRUE, when = "predict"),
      keys = c("x", "object", "arma")
    ),
    properties = c("numerics", "quantile"),
    name = "AutoRegressive Integrated Moving Average",
    short.name = "Arima",
    note = "All variables besides the target will be passed to the xreg argument.",
    callees = c("Arima", "forecast.Arima")
    )
}

#'@export
trainLearner.fcregr.Arima = function(.learner, .task, .subset, .weights = NULL, ...) {
  data = getTaskData(.task, .subset, target.extra = TRUE)
  td = getTaskDesc(.task)
  data$target = ts(data$target, start = 1, frequency = td$frequency)
  if (ncol(data$data) != 0) {
    data$data = ts(data$data, start = 1, frequency = td$frequency)
    forecast::Arima(y = data$target, xreg = data$data, ...)
  } else {
    forecast::Arima(y = data$target, ...)
  }
}

#'@export
predictLearner.fcregr.Arima = function(.learner, .model, .newdata, ...) {
  se.fit = getLearnerPredictType(.learner) == "quantile"
  model.td = getTaskDesc(.model)
  mod = getLearnerModel(.model)
  if (all(model.td$n.feat == 0)) {
    p = forecast::forecast(mod, ...)
  } else {
    .newdata = ts(.newdata, start = 1, frequency = model.td$frequency)
    p = forecast::forecast(mod, xreg = .newdata, ...)
  }
  if (!se.fit) {
    p = as.numeric(p$mean)
  } else {
    p.mean  = as.matrix(p$mean)
    p.lower = p$lower
    p.upper = p$upper
    colnames(p.mean)  = "point_forecast"
    colnames(p.lower) = stri_paste("lower_", p$level)
    colnames(p.upper) = stri_paste("upper_", p$level)
    p = cbind(p.mean, p.lower, p.upper)
  }
  return(p)
}

