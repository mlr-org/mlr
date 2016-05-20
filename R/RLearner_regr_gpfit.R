#' @export
makeRLearner.regr.gpfit <- function(){
  makeRLearnerRegr(
    cl = "regr.gpfit",
    package = "GPfit",
    par.set = makeParamSet(
      makeNumericVectorLearnerParam(id = "contro", len = 3),
      makeNumericLearnerParam(id = "nug_thres", default = 20),
      makeLogicalLearnerParam(id = "trace", default = FALSE),
      makeIntegerLearnerParam(id = "maxit", default = 100),
      makeNumericLearnerParam(id = "optim_start")
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
  GPfit::GP_fit(d$data, d$target, ...)
  }
#' @export
predictLearner.regr.gpfit = function(.learner, .model, .newdata, ...) {
  predict(.model$learner.model, xnew = .newdata)$Y_hat
}

