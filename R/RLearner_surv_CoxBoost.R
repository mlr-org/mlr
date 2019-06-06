#' @export
makeRLearner.surv.CoxBoost = function() {
  makeRLearnerSurv(
    cl = "surv.CoxBoost",
    package = "!CoxBoost",
    par.set = makeParamSet(
      makeIntegerVectorLearnerParam(id = "unpen.index"),
      makeLogicalLearnerParam(id = "standardize", default = TRUE),
      makeNumericLearnerParam(id = "penalty", lower = 0),
      makeDiscreteLearnerParam(id = "criterion", default = "pscore", values = c("pscore", "score", "hpscore", "hscore")),
      makeNumericLearnerParam(id = "stepsize.factor", default = 1, lower = 0),
      makeIntegerLearnerParam(id = "stepno", default = 100L, lower = 1),
      makeLogicalLearnerParam(id = "return.score", default = TRUE, tunable = FALSE),
      makeLogicalLearnerParam(id = "trace", default = FALSE, tunable = FALSE)
    ),
    par.vals = list(return.score = FALSE),
    properties = c("numerics", "factors", "ordered", "weights"),
    name = "Cox Proportional Hazards Model with Componentwise Likelihood based Boosting",
    short.name = "coxboost",
    note = "Factors automatically get converted to dummy columns, ordered factors to integer.",
    callees = "CoxBoost"
  )
}

#' @export
trainLearner.surv.CoxBoost = function(.learner, .task, .subset, .weights = NULL, penalty = NULL, unpen.index = NULL, ...) {
  data = getTaskData(.task, subset = .subset, target.extra = TRUE, recode.target = "surv")
  info = getFixDataInfo(data$data, factors.to.dummies = TRUE, ordered.to.int = TRUE)
  data$data = as.matrix(fixDataForLearner(data$data, info))

  if (is.null(penalty)) {
    penalty = 9 * sum(data$target[, 2L])
  }

  attachTrainingInfo(CoxBoost::CoxBoost(
    time = data$target[, 1L],
    status = data$target[, 2L],
    x = data$data,
    weights = .weights,
    penalty = penalty,
    ...
  ), info)
}

#' @export
predictLearner.surv.CoxBoost = function(.learner, .model, .newdata, ...) {
  info = getTrainingInfo(.model)
  .newdata = as.matrix(fixDataForLearner(.newdata, info))
  as.numeric(predict(.model$learner.model, newdata = .newdata, type = "lp"))
}
