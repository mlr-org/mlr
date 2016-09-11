#'@export
makeRLearner.fcregr.nnetar = function() {
  makeRLearnerForecastRegr(
    cl = "fcregr.nnetar",
    package = "forecast",
    par.set = makeParamSet(
      # no default for p
      makeIntegerLearnerParam(id = "p", lower = 0L),
      makeIntegerLearnerParam(id = "P", lower = 0L, default = 1L),
      # no default for size
      makeIntegerLearnerParam(id = "size", lower = 0L),
      makeIntegerLearnerParam(id = "repeats", lower = 1L, default = 20L),
      makeNumericLearnerParam(id = "lambda", default = NULL, special.vals = list(NULL)),
      makeUntypedLearnerParam(id = "model", default = NULL),
      makeLogicalLearnerParam(id = "scale.inputs", default = TRUE),
      # nnet params
      makeIntegerLearnerParam(id = "maxit", default = 100L, lower = 1L),
      makeLogicalLearnerParam(id = "linout", default = FALSE, requires = quote(entropy == FALSE && softmax == FALSE && censored == FALSE)),
      makeLogicalLearnerParam(id = "entropy", default = FALSE, requires = quote(linout == FALSE && softmax == FALSE && censored == FALSE)),
      makeLogicalLearnerParam(id = "softmax", default = FALSE, requires = quote(entropy == FALSE && linout == FALSE && censored == FALSE)),
      makeLogicalLearnerParam(id = "censored", default = FALSE, requires = quote(linout == FALSE && softmax == FALSE && entropy == FALSE)),
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
      makeIntegerLearnerParam(id = "h", lower = 0L, default = expression(ifelse(object$m > 1, 2 * object$m, 10)),
                             when = "predict", tunable = FALSE),
      makeLogicalLearnerParam(id = "PI", default = FALSE, tunable = FALSE, when = "predict"),
      makeNumericVectorLearnerParam(id = "level", len = NA, default = c(80, 95), when = "predict", tunable = FALSE),
      makeLogicalLearnerParam(id = "bootstrap", default = FALSE, when = "predict", tunable = FALSE),
      makeIntegerLearnerParam(id = "npaths", default = 1000, when = "predict"),
      makeUntypedLearnerParam(id = "innov", default = NULL, when = "predict"),
      # simulate params
      makeIntegerLearnerParam(id = "nsim", lower = 0L, default = expression(length(object$x)), when = "predict"),
      makeIntegerLearnerParam(id = "seed", default = NULL, special.vals = list(NULL), when = "predict"),
      makeLogicalLearnerParam(id = "future", default = TRUE, when = "predict"),
      keys = c("object", "m", "x")
      ),
    properties = c("numerics", "quantile"),
    name = "Neural Network Time Series Forecasts",
    short.name = "nnetar",
    note = "All variables besides the target will be passed to the xreg argument. Because R performs partial mapping on arguments, argument values should be passed through par.vals",
    callees = c("nnetar", "forecast.nnetar")
  )
}
#'@export
trainLearner.fcregr.nnetar = function(.learner, .task, .subset, .weights = NULL, ...) {
  data = getTaskData(.task, .subset, target.extra = TRUE)
  data$target = ts(data$target, start = 1, frequency = getTaskDesc(.task)$frequency)
  if (is.null(.weights)) {
    if (ncol(data$data) != 0) {
      data$data = ts(data$data, start = 1, frequency = getTaskDesc(.task)$frequency)
      forecast::nnetar(y = data$target, xreg = data$data, ...)
    } else {
      forecast::nnetar(y = data$target, ...)
    }
  } else {
    if (ncol(data$data) != 0) {
      data$data = ts(data$data, start = 1, frequency = .task$task.desc$frequency)
      forecast::nnetar(y = data$target, xreg = data$data, ...)
    } else {
      forecast::nnetar(y = data$target, ...)
    }
  }
}



#' @export
updateLearner.fcregr.nnetar = function(.learner, .model, .newdata, .task, .truth, .weights = NULL, ...) {
  target = getTaskTargetNames(.task)
  data = ts(.truth, start = 1, frequency = getTaskDesc(.task)$frequency)
  if (is.null(.weights)) {
    if (ncol(.newdata) == 0) {
      forecast::nnetar(y = data, model = .model$learner.model, ...)
    } else {
      .newdata = ts(.newdata, start = 1, frequency = getTaskDesc(.task)$frequency)
      forecast::nnetar(y = data, xreg = .newdata, model = .model$learner.model, ...)
    }
  } else {
    if (ncol(.newdata) != 0) {
      newdata = ts(.newdata, start = 1, frequency = .task$task.desc$frequency)
      forecast::nnetar(y = data, xreg = newdata, model = .model$learner.model, weights = .weights, ...)
    } else {
      forecast::nnetar(y = data, model = .model$learner.model, weights = .weights, ...)
    }
  }
}

#'@export
predictLearner.fcregr.nnetar = function(.learner, .model, .newdata, ...) {
  se.fit = .learner$predict.type == "quantile"
  model.td = getTaskDesc(.model)

  if (all(model.td$n.feat == 0)) {
    if (se.fit)
      p = forecast::forecast(.model$learner.model, PI = TRUE, ...)
    else
      p = forecast::forecast(.model$learner.model, ...)
  } else {
    .newdata = ts(.newdata, start = 1, frequency = .model$task.desc$frequency)
    if (se.fit)
      p = forecast::forecast(.model$learner.model, PI = TRUE, xreg = .newdata, ...)
    else
      p = forecast::forecast(.model$learner.model, xreg = .newdata, ...)
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

