#'@export
makeRLearner.fcregr.Arima = function() {
  makeRLearnerForecastRegr(
    cl = "fcregr.Arima",
    package = "forecast",
    par.set = makeParamSet(
      makeIntegerVectorLearnerParam(id = "order", len = 3L,
                                    lower = 0L, upper = Inf,
                                    default = c(0L,0L,0L),
                                    tunable = TRUE),
      makeIntegerVectorLearnerParam(id = "seasonal", len = 3L,
                                    lower = 0L, upper = Inf,
                                    default = c(0L,0L,0L),
                                    tunable = TRUE),
      makeLogicalLearnerParam(id = "include.mean", default = TRUE, tunable = TRUE),
      makeLogicalLearnerParam(id = "include.drift", default = FALSE, tunable = TRUE),
      makeNumericLearnerParam(id = "lambda", default = 1, tunable = TRUE, special.vals = list(NULL),
                              when = "both"),
      makeDiscreteLearnerParam(id = "method", values = c("CSS-ML", "ML", "CSS"),
                               default = "CSS-ML", tunable = FALSE),
      # Make prediction parameters
      makeIntegerLearnerParam(id = "h", lower = 1, upper = 1000,
                       default = 1, when = "predict", tunable = FALSE),
      makeLogicalLearnerParam(id = "biasadj", default = FALSE, tunable = TRUE, when = "predict"),
      makeLogicalLearnerParam(id = "bootstrap", default = FALSE, tunable = FALSE, when = "predict"),
      makeUntypedLearnerParam(id = "level", default = c(80,95), when = "predict"),
      makeIntegerLearnerParam(id = "npaths", default = 5000, when = "predict"),
      makeUntypedLearnerParam(id = "xreg", default = NULL)
    ),
    properties = c("numerics","ts","quantile"),
    name = "AutoRegressive Integrated Moving Average",
    short.name = "Arima",
    note = ""
    )
}

#'@export
trainLearner.fcregr.Arima = function(.learner, .task, .subset, .weights = NULL, ...) {

  data = getTaskData(.task,.subset, target.extra = TRUE)
  data$target = ts(data$target, start = 1, frequency = .task$task.desc$frequency)
  forecast::Arima(y = data$target, ...)
}

#'@export
predictLearner.fcregr.Arima = function(.learner, .model, .newdata, ...){
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


#' @export
updateLearner.fcregr.Arima = function(.learner, .model, .newdata, .task, ...){
  target = getTaskTargetNames(.task)
  data = ts(.newdata[,target], start = 1, frequency = .task$task.desc$frequency)
  forecast::Arima(y = data, model = .model$learner.model,...)
}


