#' @export
makeRLearner.surv.CoxBoost = function() {
  makeRLearnerSurv(
    cl = "surv.CoxBoost",
    package = "CoxBoost",
    par.set = makeParamSet(
      makeIntegerLearnerParam(id = "maxstepno", default = 100, lower = 0),
      makeIntegerLearnerParam(id = "K", default = 10, lower = 1),
      makeDiscreteLearnerParam(id = "type", default = "verweij", values = c("verweij", "naive")),
     #  makeIntegerLearnerParam(id = "stepno", default = NULL, lower = 1),
     #  makeNumericLearnerParam(id = "penalty", default = NULL, lower = 0),
      makeLogicalLearnerParam(id = "standardize", default = TRUE),
      makeDiscreteLearnerParam(id = "criterion", default = "pscore", values = c("pscore", "score", "hpscore", "hscore")),
      makeNumericLearnerParam(id = "stepsize.factor", default = 1, lower = 0),
      makeDiscreteLearnerParam(id = "sf.scheme", default = "sigmoid", values = c("sigmoid", "linear"))
      # FIXME: still missing some arguments
    ),
    properties = c("numerics", "weights", "rcens"),
    name = "Cox Proportional Hazards Model with Componentwise Likelihood based Boosting",
    short.name = "CoxBoost",
    note = ""
  )
}

#' @export
trainLearner.surv.CoxBoost = function(.learner, .task, .subset, .weights = NULL,  ...) {
  # FIXME: use model.matrix to allow factors
  data = getTaskData(.task, subset = .subset, target.extra = TRUE, recode.target = "rcens")
  if (is.null(.weights))
    .weights = NULL

  cb = optimCoxBoostPenalty(
    time = data$target[, 1L],
    status = data$target[, 2L],
    x = as.matrix(data$data),
    weights = .weights,
    ...
  )

  if (cb$cv.res$optimal.step == 0L)
    stop("Error modeling CoxBoost")

  CoxBoost(
    time = data$target[, 1L],
    status = data$target[, 2L],
    x = as.matrix(data$data),
    weights = .weights,
    stepno = cb$cv.res$optimal.step,
    penalty = cb$penalty,
    ...
  )
}

#' @export
predictLearner.surv.CoxBoost = function(.learner, .model, .newdata, ...) {
  if(.learner$predict.type == "response")
    as.numeric(predict(.model$learner.model, newdata = as.matrix(.newdata), type = "lp"))
  else
    stop("Unknown predict type")
}
