#'@export
makeRLearner.fcregr.ets = function() {
  makeRLearnerForecastRegr(
    cl = "fcregr.ets",
    package = "forecast",
    par.set = makeParamSet(
      makeDiscreteLearnerParam(id = "model", default = "ZZZ",
                               values = c("ANN", "MNN", "ZNN", "AAN", "MAN",
                                          "ZAN", "AMN", "MMN", "ZMN", "AZN",
                                          "MZN", "ZZN", "ANA", "MNA", "ZNA",
                                          "AAA", "MAA", "ZAA", "AMA", "MMA",
                                          "ZMA", "AZA", "MZA", "ZZA", "ANM",
                                          "MNM", "ZNM", "AAM", "MAM", "ZAM",
                                          "AMM", "MMM", "ZMM", "AZM", "MZM",
                                          "ZZM", "ANZ", "MNZ", "ZNZ", "AAZ",
                                          "MAZ", "ZAZ", "AMZ", "MMZ", "ZMZ",
                                          "AZZ", "MZZ", "ZZZ")),
      makeLogicalLearnerParam(id = "damped", default = NULL, tunable = TRUE, special.vals = list(NULL)),
      makeNumericLearnerParam(id = "alpha", default = NULL, special.vals = list(NULL)),
      makeNumericLearnerParam(id = "beta", default = NULL, special.vals = list(NULL)),
      makeNumericLearnerParam(id = "gamma", default = NULL, special.vals = list(NULL)),
      makeNumericLearnerParam(id = "phi", default = NULL, special.vals = list(NULL)),
      makeLogicalLearnerParam(id = "additive.only", default = FALSE),
      makeNumericLearnerParam(id = "lambda", default = NULL, special.vals = list(NULL)),
      makeLogicalLearnerParam(id = "biasadj", default = FALSE),
      makeNumericVectorLearnerParam(id = "lower", len = 4L, default = c(rep(0.0001, 3), 0.8)),
      makeNumericVectorLearnerParam(id = "upper", len = 4L, default = c(rep(0.9999, 3), 0.98)),
      makeDiscreteLearnerParam(id = "opt.crit", values = c("mse", "amse", "sigma", "mae", "lik"), default = "lik"),
      makeIntegerLearnerParam(id = "nmse", lower = 1, upper = 30, default = 3),
      makeDiscreteLearnerParam(id = "bounds", values = c("usual", "admissable", "both"), default = "both"),
      makeDiscreteLearnerParam(id = "ic", values = c("aicc", "aic", "bic"), default = "aicc"),
      makeLogicalLearnerParam(id = "restrict", default = TRUE),
      makeLogicalLearnerParam(id = "allow.multiplicative.trend", default = FALSE),
      makeLogicalLearnerParam(id = "use.initial.values", default = FALSE),
      # prediction
      makeIntegerLearnerParam(id = "h", lower = 0, upper = Inf, default = expression(ifelse(object$m > 1, 2 * object$m, 10)),
                              tunable = FALSE, when = "predict"),
      makeLogicalLearnerParam(id = "bootstrap", default = FALSE, when = "predict", tunable = FALSE),
      makeNumericVectorLearnerParam(id = "level", len = NA, default = c(80, 95), when = "predict", tunable = FALSE),
      makeLogicalLearnerParam(id = "fan", default = FALSE, when = "predict", tunable = FALSE),
      makeLogicalLearnerParam(id = "simulate", default = FALSE, when = "predict", tunable = FALSE),
      makeLogicalLearnerParam(id = "PI", default = TRUE, when = "predict", tunable = FALSE),
      makeIntegerLearnerParam(id = "npaths", default = 5000, when = "predict"),
      # simulate params
      makeIntegerLearnerParam(id = "nsim", lower = 0L, default = expression(length(object$x))),
      makeIntegerLearnerParam(id = "seed", default = NULL, special.vals = list(NULL)),
      makeLogicalLearnerParam(id = "future", default = TRUE),
      keys = c("x", "object", "m")
    ),
    properties = c("numerics", "quantile"),
    name = "Exponential smoothing state space model",
    short.name = "ets",
    callees = c("ets", "forecast.ets")
  )
}

#'@export
trainLearner.fcregr.ets = function(.learner, .task, .subset, .weights = NULL, ...) {

  data = getTaskData(.task, .subset, target.extra = TRUE)
  data$target = ts(data$target, start = 1, frequency = getTaskDesc(.task)$frequency)
  forecast::ets(y = data$target, ...)
}

#' @export
updateLearner.fcregr.ets = function(.learner, .model, .newdata = NULL, .task, .truth, .weights = NULL, ...) {
  target = getTaskTargetNames(.task)
  data = ts(.truth, start = 1, frequency = getTaskDesc(.task)$frequency)
  forecast::ets(y = data, model = .model$learner.model)
}

#'@export
predictLearner.fcregr.ets = function(.learner, .model, .newdata, ...) {
  se.fit = .learner$predict.type == "quantile"
  if (!se.fit) {
    p = as.numeric(forecast::forecast(object = .model$learner.model, ...)$mean)
  } else {
    pse = forecast::forecast(object = .model$learner.model, ...)
    p.mean  = as.matrix(pse$mean)
    p.lower = pse$lower
    p.upper = pse$upper
    colnames(p.mean)  = "point_forecast"
    colnames(p.lower) = stri_paste("lower_", pse$level)
    colnames(p.upper) = stri_paste("upper_", pse$level)
    p = cbind(p.mean, p.lower, p.upper)
  }
  return(p)
}
