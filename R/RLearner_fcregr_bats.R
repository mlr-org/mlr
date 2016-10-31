#'@export
makeRLearner.fcregr.bats = function() {
  makeRLearnerForecastRegr(
    cl = "fcregr.bats",
    package = "forecast",
    par.set = makeParamSet(
      makeLogicalLearnerParam(id = "use.box.cox", default = FALSE, tunable = TRUE),
      makeLogicalLearnerParam(id = "use.trend", default = FALSE, tunable = TRUE),
      makeLogicalLearnerParam(id = "use.damped.trend", default = FALSE, tunable = TRUE),
      makeUntypedLearnerParam(id = "seasonal.periods", default = NULL),
      makeLogicalLearnerParam(id = "use.arma.errors", default = TRUE, tunable = TRUE),
      makeLogicalLearnerParam(id = "use.parallel", default = FALSE, tunable = TRUE),
      makeNumericLearnerParam(id = "bc.lower", lower = -Inf, upper = Inf, default = 0),
      makeNumericLearnerParam(id = "bc.upper", lower = -Inf, upper = Inf, default = 1),
      makeUntypedLearnerParam(id = "model", default = NULL),
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
      makeLogicalLearnerParam(id = "stationary", default = FALSE, tunable = TRUE),
      makeLogicalLearnerParam(id = "seasonal", default = TRUE, tunable = TRUE),
      makeDiscreteLearnerParam(id = "ic", values = c("aicc", "aic", "bic"), default = "aicc"),
      makeLogicalLearnerParam(id = "stepwise", default = FALSE, tunable = TRUE),
      makeLogicalLearnerParam(id = "trace", default = FALSE, tunable = FALSE),
      makeLogicalLearnerParam(id = "approximation"),
      makeIntegerLearnerParam(id = "truncate", lower = 1, upper = Inf),
      makeDiscreteLearnerParam(id = "test", values = c("kpss","adf","pp"), default = "kpss"),
      makeDiscreteLearnerParam(id = "seasonal.test", values = c("ocsb", "ch")),
      makeLogicalLearnerParam(id = "allowdrift", default = TRUE, tunable = FALSE),
      makeLogicalLearnerParam(id = "allowmean", default = TRUE, tunable = FALSE),
      makeNumericLearnerParam(id = "lambda"),
      makeLogicalLearnerParam(id = "biasadj", default = FALSE, tunable = TRUE),
      # prediction
      makeIntegerLearnerParam(id = "h", lower = 0, upper = Inf, default = 1, tunable = FALSE,
                              when = "predict"),
      makeLogicalLearnerParam(id = "bootstrap", default = FALSE, tunable = FALSE, when = "predict"),
      makeUntypedLearnerParam(id = "level", default = c(80,95), when = "predict"),
      makeIntegerLearnerParam(id = "npaths", default = 5000, when = "predict")
    ),
    properties = c("numerics","quantile"),
    name = "Exponential smoothing state space model with Box-Cox transformation,
    ARMA errors, Trend and Seasonal components",
    short.name = "bats",
    note = ""
  )
}

#'@export
trainLearner.fcregr.bats = function(.learner, .task, .subset, .weights = NULL, ...) {

  data = getTaskData(.task,.subset,target.extra = TRUE)
  data$target = ts(data$target, start = 1, frequency = .task$task.desc$frequency)
  forecast::bats(y = data$target, ...)
}

#' @export
updateLearner.fcregr.bats = function(.learner, .model, .newdata, .task, ...){
  target = getTaskTargetNames(.task)
  data = ts(.newdata[,target], start = 1, frequency = .task$task.desc$frequency)
  forecast::bats(y = data, model = .model$learner.model,...)
}

#'@export
predictLearner.fcregr.bats = function(.learner, .model, .newdata, ...){
  se.fit = .learner$predict.type == "quantile"
  if (!se.fit){
    p = as.numeric(forecast::forecast(.model$learner.model, ...)$mean)
  } else {
    pse = forecast::forecast(.model$learner.model, ...)
    pMean  = as.matrix(pse$mean)
    pLower = pse$lower
    pUpper = pse$upper
    colnames(pMean)  = "point_forecast"
    colnames(pLower) = paste0("lower_",pse$level)
    colnames(pUpper) = paste0("upper_",pse$level)
    p = cbind(pMean,pLower,pUpper)
  }
  return(p)
}
