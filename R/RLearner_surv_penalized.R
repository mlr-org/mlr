#' @export
makeRLearner.surv.penalized = function() {
  makeRLearnerSurv(
    cl = "surv.penalized",
    package = "penalized",
    par.set = makeParamSet(
      makeNumericLearnerParam(id = "lambda1", default = 0, lower = 0),
      makeNumericLearnerParam(id = "lambda2", default = 0, lower = 0),
      makeLogicalLearnerParam(id = "fusedl", default = FALSE),
      makeLogicalLearnerParam(id = "standardize", default = FALSE),
      makeIntegerLearnerParam(id = "maxiter", default = 25L),
      makeLogicalLearnerParam(id = "trace", default = FALSE, tunable = FALSE)
    ),
    properties = c("numerics", "factors", "ordered", "rcens"),
    name = "Penalized Regression",
    short.name = "penalized",
    note = "Factors automatically get converted to dummy columns, ordered factors to integer."
  )
}

#' @export
trainLearner.surv.penalized = function(.learner, .task, .subset, .weights = NULL,  ...) {
  data = getTaskData(.task, subset = .subset, target.extra = TRUE, recode.target = "rcens")
  info = getFixDataInfo(data$data, factors.to.dummies = TRUE, ordered.to.int = TRUE)
  data$data = as.matrix(fixDataForLearner(data$data, info))

  attachTrainingInfo(
    penalized::penalized(response = data$target, penalized = data$data, model = "cox", trace = FALSE, ...),
    info
  )
}

#' @export
predictLearner.surv.penalized = function(.learner, .model, .newdata, ...) {
  info = getTrainingInfo(.model)
  .newdata = as.matrix(fixDataForLearner(.newdata, info))
  if (.learner$predict.type == "response") {
    # Note: this is a rather ugly hack but should work according to Jelle
    penalized::survival(penalized::predict(.model$learner.model, penalized = .newdata), Inf)
  } else {
    stop("Unknown predict type")
  }
}
