#' @export
makeRLearner.surv.optimCoxBoostPenalty = function() {
  makeRLearnerSurv(
    cl = "surv.optimCoxBoostPenalty",
    package = "CoxBoost",
    par.set = makeParamSet(
      makeIntegerLearnerParam(id = "maxstepno", default = 100, lower = 0),
      makeIntegerLearnerParam(id = "K", default = 10, lower = 1),
      makeDiscreteLearnerParam(id = "type", default = "verweij", values = c("verweij", "naive")),
      makeLogicalLearnerParam(id = "standardize", default = TRUE),
      makeDiscreteLearnerParam(id = "criterion", default = "pscore", values = c("pscore", "score", "hpscore", "hscore")),
      makeNumericLearnerParam(id = "stepsize.factor", default = 1, lower = 0),
      makeDiscreteLearnerParam(id = "sf.scheme", default = "sigmoid", values = c("sigmoid", "linear"))
      # FIXME: still missing some arguments
    ),
    properties = c("numerics", "weights", "rcens"),
    name = "Cox Proportional Hazards Model with Componentwise Likelihood based Boosting, automatic tuning enabled",
    short.name = "optimCoxBoostPenalty",
    note = "Factors automatically get converted to dummy columns, ordered factors to integer"
  )
}

#' @export
trainLearner.surv.optimCoxBoostPenalty = function(.learner, .task, .subset, .weights = NULL, ...) {
  data = getTaskData(.task, subset = .subset, target.extra = TRUE, recode.target = "rcens")
  info = getFixDataInfo(data$data, factors.to.dummies = TRUE, ordered.to.int = TRUE)
  data$data = as.matrix(fixDataForLearner(data$data, info))

  cb = CoxBoost::optimCoxBoostPenalty(
    time = data$target[, 1L],
    status = data$target[, 2L],
    x = data$data,
    weights = .weights,
    ...
  )

  if (cb$cv.res$optimal.step == 0L)
    stop("Error modeling CoxBoost")

  attachTrainingInfo(CoxBoost::CoxBoost(
    time = data$target[, 1L],
    status = data$target[, 2L],
    x = data$data,
    weights = .weights,
    stepno = cb$cv.res$optimal.step,
    penalty = cb$penalty,
    ...
  ), info)
}

#' @export
predictLearner.surv.optimCoxBoostPenalty = function(.learner, .model, .newdata, ...) {
  info = getTrainingInfo(.model)
  .newdata = as.matrix(fixDataForLearner(.newdata, info))
  if(.learner$predict.type == "response")
    as.numeric(predict(.model$learner.model, newdata = .newdata, type = "lp"))
  else
    stop("Unknown predict type")
}
