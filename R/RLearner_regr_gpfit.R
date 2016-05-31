#' @export
makeRLearner.regr.gpfit <- function(){
  makeRLearnerRegr(
    cl = "regr.gpfit",
    package = "GPfit",
    par.set = makeParamSet(
      makeNumericVectorLearnerParam(id = "control", len = 3, lower = c(1, 1, 1)),
      makeNumericLearnerParam(id = "nug_thres", default = 20, lower = 10, upper = 25),
      makeLogicalLearnerParam(id = "trace", default = FALSE, tunable = FALSE),
      makeIntegerLearnerParam(id = "maxit", default = 100, lower = 0),
      makeUntypedLearnerParam(id = "optim_start", tunable = FALSE)
    ),
    properties = c("numerics"),
    name = "Gaussian Process Model fitting",
    short.name = "gpfit",
    note = "The optimization routine assumes that the inputs are scaled to the unit hypercube [0,1]^d"
  )
}
#' @export
trainLearner.regr.gpfit = function(.learner, .task, .subset, ...) {
  d = getTaskData(.task, .subset, target.extra = TRUE)
  low = min(d$data)
  high = max(d$data)
  if (low < 0 | high > 1) {
    d$data = (d$data - min(d$data)) / (max(d$data) - min(d$data))
    res = GPfit::GP_fit(d$data, d$target, ...)
    res = attachTrainingInfo(res, list(scaled = TRUE, high = high, low = low))
    return(res)
  } else {
    res = GPfit::GP_fit(d$data, d$target, ...)
    res = attachTrainingInfo(res, list(scaled = FALSE, high = high, low = low))
    return(res)
  }
}
#' @export
predictLearner.regr.gpfit = function(.learner, .model, .newdata, ...) {
  sc.param = getTrainingInfo(.model)
  if (sc.param$scaled == TRUE) {
    newdata = (.newdata - sc.param$low) / (sc.param$high - sc.param$low)
  } else {
    newdata = .newdata
  }
  predict(.model$learner.model, xnew = newdata)$Y_hat
}

