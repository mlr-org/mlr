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
  if (scale) {
    low = apply(d$data, 2, min)
    high = apply(d$data, 2, max)
    const = which(high == low)
    if (length(const > 0)) {
      d$data = apply(d$data[,-const], 2, function(x) x = (x - min(x)) / (max(x) - min(x)))
    } else {
      d$data = apply(d$data, 2, function(x) x = (x - min(x)) / (max(x) - min(x)))
    }
    res = GPfit::GP_fit(d$data, d$target, ...)
    res = attachTrainingInfo(res, list(scaled = TRUE, const = const, high = high, low = low))
    return(res)
  } else {
    res = GPfit::GP_fit(d$data, d$target, ...)
    res = attachTrainingInfo(res, list(scaled = FALSE))
    return(res)
  }
}
#' @export
predictLearner.regr.gpfit = function(.learner, .model, .newdata, ...) {
  tr.info = getTrainingInfo(.model)
  if (tr.info$scaled) {
    if (length(tr.info$const) > 0) {
      inds = (1:ncol(.newdata))[-tr.info$const]
    } else {
      inds = 1:ncol(.newdata)
    }
      for (i in inds) {
        .newdata[,i] =  (.newdata[,i] - tr.info$low[i]) / (tr.info$high[i] - tr.info$low[i])
    }
    if (length(tr.info$const) > 0) {
      .newdata = .newdata[,-tr.info$const]
    }
  } 
  predict(.model$learner.model, xnew = .newdata)$Y_hat
}

