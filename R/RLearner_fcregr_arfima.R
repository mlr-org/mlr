#'@export
#' @importFrom xts try.xts reclass
makeRLearner.fcregr.arfima = function() {
  makeRLearnerForecastRegr(
    cl = "fcregr.arfima",
    package = "forecast",
    par.set = makeParamSet(
      makeNumericVectorLearnerParam(id = "drange", len = 2L, lower = 0, default = c(0, 0.5)),
      makeDiscreteLearnerParam(id = "estim", values = c("mle", "ls"), default = "mle"),
      makeUntypedLearnerParam(id = "model", default = NULL),
      # auto.arima params
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
      makeLogicalLearnerParam(id = "biasadj", default = FALSE, when = "train"),
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
      makeIntegerLearnerParam(id = "h", lower = 0, upper = Inf, default = 10, tunable = FALSE, when = "predict"),
      makeNumericVectorLearnerParam(id = "level", len = NA, default = c(80, 95), when = "predict", tunable = FALSE),
      makeLogicalLearnerParam(id = "fan", default = FALSE, when = "predict", tunable = FALSE),
      # simulate params
      makeIntegerLearnerParam(id = "nsim", lower = 0L, default = expression(length(object$n))),
      makeIntegerLearnerParam(id = "seed", default = NULL, special.vals = list(NULL)),
      makeLogicalLearnerParam(id = "future", default = TRUE),
      keys = c("x", "object", "arma", "n")
    ),
    properties = c("numerics", "quantile"),
    name = "AutoRegressive Fractionally Integrated Moving Average",
    short.name = "arfima",
    note = "All variables besides the target will be passed to the xreg argument."
  )
}

#'@export
trainLearner.fcregr.arfima = function(.learner, .task, .subset, .weights = NULL, ...) {

  data = getTaskData(.task, .subset, target.extra = TRUE)
  data$target = ts(data$target, start = 1, frequency = .task$task.desc$frequency)
  forecast::arfima(y = data$target, ...)
}

#'@export
predictLearner.fcregr.arfima = function(.learner, .model, .newdata, ...) {
  se.fit = .learner$predict.type == "quantile"
  if (!se.fit) {
    p = as.numeric(forecast::forecast(.model$learner.model, ...)$mean)
  } else {
    pse = forecast::forecast(.model$learner.model, ...)
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

