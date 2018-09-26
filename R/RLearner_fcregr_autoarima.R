makeRLearner.fcregr.auto.arima = function() {
  makeRLearnerForecastRegr(
    cl = "fcregr.auto.arima",
    package = "forecast",
    par.set = makeParamSet(
      makeIntegerLearnerParam(id = "d", lower = 0, upper = Inf),
      makeIntegerLearnerParam(id = "D", lower = 0, upper = Inf),
      makeIntegerLearnerParam(id = "max.p", lower = 0, upper = Inf, default = 5),
      makeIntegerLearnerParam(id = "max.q", lower = 0, upper = Inf, default = 5),
      makeIntegerLearnerParam(id = "max.P", lower = 0, upper = Inf, default = 2),
      makeIntegerLearnerParam(id = "max.Q", lower = 0, upper = Inf, default = 2),
      makeIntegerLearnerParam(id = "max.order", lower = 0, upper = Inf, default = 5),
      makeIntegerLearnerParam(id = "max.d", lower = 0, upper = Inf, default = 2),
      makeIntegerLearnerParam(id = "max.D", lower = 0, upper = Inf, default = 1),
      makeIntegerLearnerParam(id = "start.p", lower = 0, upper = Inf, default = 2),
      makeIntegerLearnerParam(id = "start.q", lower = 0, upper = Inf, default = 2),
      makeIntegerLearnerParam(id = "start.P", lower = 0, upper = Inf, default = 1),
      makeIntegerLearnerParam(id = "start.Q", lower = 0, upper = Inf, default = 1),
      makeLogicalLearnerParam(id = "stationary", default = FALSE),
      makeLogicalLearnerParam(id = "seasonal", default = TRUE),
      makeDiscreteLearnerParam(id = "ic", values = c("aicc", "aic", "bic"), default = "aicc"),
      makeLogicalLearnerParam(id = "stepwise", default = TRUE),
      makeLogicalLearnerParam(id = "trace", default = FALSE),
      makeLogicalLearnerParam(id = "approximation", default = expression(length(x) > 100 | frequency(x) > 12)),
      makeIntegerLearnerParam(id = "truncate", lower = 1, default = NULL, special.vals = list(NULL)),
      makeDiscreteLearnerParam(id = "test", values = c("kpss", "adf", "pp"), default = "kpss"),
      makeDiscreteLearnerParam(id = "seasonal.test", values = c("ocsb", "ch"), default = "ocsb"),
      makeLogicalLearnerParam(id = "allowdrift", default = TRUE),
      makeLogicalLearnerParam(id = "allowmean", default = TRUE),
      makeNumericLearnerParam(id = "lambda", default = NULL, special.vals = list(NULL), when = "both"),
      makeLogicalLearnerParam(id = "biasadj", default = FALSE, when = "both"),
      makeIntegerLearnerParam(id = "num.cores", lower = 0, upper = Inf, tunable = FALSE, default = 2),
      # arima params
      makeLogicalLearnerParam(id = "transform.pars", default = TRUE),
      makeNumericVectorLearnerParam(id = "fixed", len = NA, default = NULL, special.vals = list(NULL)),
      makeNumericVectorLearnerParam(id = "init", len = NA, default = NULL, special.vals = list(NULL)),
      # No default
      makeIntegerLearnerParam("n.cond", lower = 0),
      makeDiscreteLearnerParam("SSinit", values = c("Gardner1980", "Rossignol2011"), default = "Gardner1980", tunable = FALSE),
      makeDiscreteLearnerParam("optim.method", default = "BFGS", values = c("Nelder-Mead", "BFGS",
                                                                            "CG", "L-BFGS-B",
                                                                            "SANN", "Brent"),
                               tunable = FALSE),
      makeUntypedLearnerParam("optim.controls", default = list(), tunable = FALSE),
      makeNumericLearnerParam("kappa", lower = 1e6, upper = Inf, tunable = FALSE),
      # prediction
      makeIntegerLearnerParam(id = "h", lower = 0, upper = Inf, default = expression(ifelse(object$arma[5] > 1, 2 * object$arma[5], 10)), tunable = FALSE,
                              when = "predict"),
      makeLogicalLearnerParam(id = "bootstrap", default = FALSE, when = "predict", tunable = FALSE),
      makeNumericVectorLearnerParam(id = "level", len = NA, default = c(80, 95), when = "predict", tunable = FALSE),
      makeLogicalLearnerParam(id = "fan", default = FALSE, when = "predict", tunable = FALSE),
      makeIntegerLearnerParam(id = "npaths", default = 5000, when = "predict"),
      # simulate params
      makeIntegerLearnerParam(id = "nsim", lower = 0L, default = expression(length(object$x))),
      makeIntegerLearnerParam(id = "seed", default = NULL, special.vals = list(NULL)),
      makeLogicalLearnerParam(id = "future", default = TRUE),
      keys = c("x", "object", "arma")
    ),
    properties = c("numerics", "quantile"),
    name = "Automated Autoregressive Integrated Moving Average Model Selection",
    short.name = "auto.arima",
    note = "All variables besides the target will be passed to the xreg argument.",
    callees = c("auto.arima", "forecast.Arima")
  )
}

#'@export
trainLearner.fcregr.auto.arima = function(.learner, .task, .subset, .weights = NULL, ...) {

  data = getTaskData(.task, .subset, target.extra = TRUE)
  data$target = ts(data$target, start = 1, frequency = .task$task.desc$frequency)
  if (ncol(data$data) != 0) {
    data$data = ts(data$data, start = 1, frequency = .task$task.desc$frequency)
    forecast::auto.arima(y = data$target, xreg = data$data, ...)
  } else {
    forecast::auto.arima(y = data$target, ...)
  }
}


#' @export
updateLearner.fcregr.auto.arima = function(.learner, .model, .newdata, .task, .truth, .weights = NULL, ...) {

  target = ts(.truth, start = 1, frequency = .task$task.desc$frequency)
  if (ncol(.newdata) == 0) {
    updated = forecast::Arima(y = target, model = .model$learner.model)
  } else {
    xdata = ts(.newdata, start = 1, frequency = .task$task.desc$frequency)
    updated = forecast::Arima(y = target, model = .model$learner.model, xreg = xdata )
  }
  return(updated)
}

#'@export
predictLearner.fcregr.auto.arima = function(.learner, .model, .newdata, ...) {
  se.fit = .learner$predict.type == "quantile"
  model.td = getTaskDesc(.model)

  if (all(model.td$n.feat == 0)) {
    p = forecast::forecast(.model$learner.model, ...)
  } else {
    .newdata = ts(.newdata, start = 1, frequency = .model$task.desc$frequency)
    p = forecast::forecast(.model$learner.model, xreg = .newdata, ...)
  }
  if (!se.fit) {
    p = as.numeric(p$mean)
  } else {
    p.mean  = as.matrix(p$mean)
    p.lower = p$lower
    p.upper = p$upper
    colnames(p.mean) = "point_forecast"
    colnames(p.lower) = stri_paste("lower_", p$level)
    colnames(p.upper) = stri_paste("upper_", p$level)
    p = cbind(p.mean, p.lower, p.upper)
  }
  return(p)

}
