#'@export
#' @importFrom xts try.xts reclass
makeRLearner.fcregr.arfima = function() {
  makeRLearnerForecastRegr(
    cl = "fcregr.arfima",
    package = "forecast",
    par.set = makeParamSet(
      makeNumericVectorLearnerParam(id = "drange", len = 2L, lower = 0, default = c(0,0.5)),
      makeDiscreteLearnerParam(id = "estim", values = c("mle","ls"), default = "mle"),
      makeLogicalLearnerParam(id = "parallel", default = FALSE, tunable = FALSE),
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
    name = "AutoRegressive Fractionally Integrated Moving Average",
    short.name = "arfima",
    note = ""
  )
}

#'@export
trainLearner.fcregr.arfima = function(.learner, .task, .subset, .weights = NULL, ...) {

  data = getTaskData(.task,.subset,target.extra = TRUE)
  data$target = ts(data$target, start = 1, frequency = .task$task.desc$frequency)
  forecast::arfima(y = data$target, ...)
}

#'@export
predictLearner.fcregr.arfima = function(.learner, .model, .newdata, ...){
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

