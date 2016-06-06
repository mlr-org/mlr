#' @export
makeRLearner.regr.gpfit = function(){
  makeRLearnerRegr(
    cl = "regr.gpfit",
    package = "GPfit",
    par.set = makeParamSet(
      makeNumericVectorLearnerParam(id = "control", len = 3, lower = c(1, 1, 1)),
      makeNumericLearnerParam(id = "nug_thres", default = 20, lower = 10, upper = 25),
      makeLogicalLearnerParam(id = "trace", default = FALSE, tunable = FALSE),
      makeIntegerLearnerParam(id = "maxit", default = 100, lower = 0),
      makeUntypedLearnerParam(id = "optim_start", tunable = FALSE),
      makeLogicalLearnerParam(id = "scale", default = TRUE)
    ),
    properties = c("numerics"),
    name = "Gaussian Process Model fitting",
    short.name = "gpfit",
    note = "As the optimization routine assumes that the inputs are scaled to the unit hypercube [0,1]^d, 
            the input gets scaled for each variable by default. If this is not wanted, scale = FALSE has
            to be set."
  )
}
#' @export
trainLearner.regr.gpfit = function(.learner, .task, .subset, scale = TRUE, ...) {
  d = getTaskData(.task, .subset, target.extra = TRUE)
  low = apply(d$data, 2, min)
  high = apply(d$data, 2, max)
  not.const = colnames(d$data)[high != low]
  if (scale) {
    d$data[,not.const] = apply(d$data[,not.const], 2, function(x) x = (x - min(x)) / (max(x) - min(x)))
    res = GPfit::GP_fit(d$data[, not.const], d$target, ...)
    res = attachTrainingInfo(res, list(scaled = TRUE, not.const = not.const, high = high, low = low))
    return(res)
  } else {
    res = GPfit::GP_fit(d$data[, not.const], d$target, ...)
    res = attachTrainingInfo(res, list(scaled = FALSE, not.const = not.const))
    return(res)
  }
}
#' @export
predictLearner.regr.gpfit = function(.learner, .model, .newdata, ...) {
  tr.info = getTrainingInfo(.model)
  if (tr.info$scaled) {
      for (col.name in tr.info$not.const) {
        .newdata[,col.name] =  (.newdata[,col.name] - tr.info$low[col.name]) / (tr.info$high[col.name] - tr.info$low[col.name])
    }
  } 
  predict(.model$learner.model, xnew = .newdata[, tr.info$not.const])$Y_hat
}

