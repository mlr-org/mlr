#'@export
makeRLearner.fcregr.nnetar = function() {
  makeRLearnerForecastRegr(
    cl = "fcregr.nnetar",
    package = "forecast",
    par.set = makeParamSet(
      makeIntegerLearnerParam(id = "p", lower = 0L),
      makeIntegerLearnerParam(id = "P", lower = 0L, default = 1L),
      makeIntegerLearnerParam(id = "size", lower = 0L),
      makeIntegerLearnerParam(id = "repeats", lower = 1L, default = 20L),
      makeUntypedLearnerParam(id = "xreg", default = NULL, tunable = FALSE),
      makeNumericLearnerParam(id = "lambda"),
      makeUntypedLearnerParam(id = "model", default = NULL),
      makeLogicalLearnerParam(id = "scale.inputs", default = TRUE),
      # nnet params
      makeIntegerLearnerParam(id = "maxit", default = 100L, lower = 1L),
      makeLogicalLearnerParam(id = "linout", default = FALSE, requires = quote(entropy==FALSE && softmax==FALSE && censored==FALSE)),
      makeLogicalLearnerParam(id = "entropy", default = FALSE, requires = quote(linout==FALSE && softmax==FALSE && censored==FALSE)),
      makeLogicalLearnerParam(id = "softmax", default = FALSE, requires = quote(entropy==FALSE && linout==FALSE && censored==FALSE)),
      makeLogicalLearnerParam(id = "censored", default = FALSE, requires = quote(linout==FALSE && softmax==FALSE && entropy==FALSE)),
      makeLogicalLearnerParam(id = "skip", default = FALSE),
      makeNumericLearnerParam(id = "rang", default = 0.7),
      makeNumericLearnerParam(id = "decay", default = 0, lower = 0),
      makeLogicalLearnerParam(id = "Hess", default = FALSE),
      makeLogicalLearnerParam(id = "trace", default = TRUE, tunable = FALSE),
      makeIntegerLearnerParam(id = "MaxNWts", default = 1000L, lower = 1L),
      # FIXME_PK: Why are abstoll and reltoll written with 2 "l"?
      makeNumericLearnerParam(id = "abstol", default = 1.0e-4),
      makeNumericLearnerParam(id = "reltol", default = 1.0e-8),
      # predict params
      makeIntegerLearnerParam(id = "h", lower = 0L, default = 1L, when = "predict"),
      makeLogicalLearnerParam(id = "bootstrap", default = FALSE, tunable = FALSE, when = "predict"),
      makeUntypedLearnerParam(id = "level", default = c(80,95), when = "predict"),
      makeIntegerLearnerParam(id = "npaths", default = 1000, when = "predict")
      ),
    properties = c("numerics", "quantile"),
    name = "Neural Network Time Series Forecasts",
    short.name = "nnetar",
    note = ""
  )
}
#'@export
trainLearner.fcregr.nnetar = function(.learner, .task, .subset, .weights = NULL, ...) {
  data = getTaskData(.task,.subset,target.extra = TRUE)
  data$target = ts(data$target, start = 1, frequency = .task$task.desc$frequency)
  if (is.null(.weights)){
    forecast::nnetar(y = data$target, ...)
  } else {
    forecast::nnetar(y = data$target, weights = .weights,...)
  }
}

#' @export
updateLearner.fcregr.nnetar = function(.learner, .model, .newdata, .task, .weights = NULL, ...){
  target = getTaskTargetNames(.task)
  data = ts(.newdata[,target], start = 1, frequency = .task$task.desc$frequency)
  if (is.null(weights)){
    forecast::nnetar(y = data, model = .model$learner.model,...)
  } else {
    forecast::nnetar(y = data, model = .model$learner.model, weights = .weights, ...)
  }
}

#'@export
predictLearner.fcregr.nnetar = function(.learner, .model, .newdata, ...){
  se.fit = .learner$predict.type == "quantile"
  if (!se.fit){
    p = as.numeric(forecast::forecast(.model$learner.model, ...)$mean)
  } else {
    pse = forecast::forecast(.model$learner.model, PI = TRUE, ...)
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

