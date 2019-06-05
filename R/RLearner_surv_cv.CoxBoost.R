#' @export
makeRLearner.surv.cv.CoxBoost = function() {
  makeRLearnerSurv(
    cl = "surv.cv.CoxBoost",
    package = "!CoxBoost",
    par.set = makeParamSet(
      makeIntegerLearnerParam(id = "maxstepno", default = 100L, lower = 0L),
      makeIntegerLearnerParam(id = "K", default = 10L, lower = 1L),
      makeDiscreteLearnerParam(id = "type", default = "verweij", values = c("verweij", "naive")),
      makeLogicalLearnerParam(id = "parallel", default = FALSE, tunable = FALSE),
      makeLogicalLearnerParam(id = "upload.x", default = FALSE, tunable = FALSE),
      makeLogicalLearnerParam(id = "multicore", default = FALSE, tunable = FALSE),
      makeIntegerVectorLearnerParam(id = "unpen.index"),
      makeLogicalLearnerParam(id = "standardize", default = TRUE),
      makeNumericLearnerParam(id = "penalty", lower = 0),
      makeDiscreteLearnerParam(id = "criterion", default = "pscore", values = c("pscore", "score", "hpscore", "hscore")),
      makeNumericLearnerParam(id = "stepsize.factor", default = 1, lower = 0),
      makeLogicalLearnerParam(id = "trace", default = FALSE, tunable = FALSE)
    ),
    properties = c("numerics", "factors", "weights"),
    name = "Cox Proportional Hazards Model with Componentwise Likelihood based Boosting, tuned for the optimal number of boosting steps",
    short.name = "cv.CoxBoost",
    note = "Factors automatically get converted to dummy columns, ordered factors to integer.",
    callees = c("cv.CoxBoost", "CoxBoost")
  )
}

#' @export
trainLearner.surv.cv.CoxBoost = function(.learner, .task, .subset, .weights = NULL, penalty = NULL, unpen.index = NULL, ...) {

  data = getTaskData(.task, subset = .subset, target.extra = TRUE, recode.target = "surv")
  info = getFixDataInfo(data$data, factors.to.dummies = TRUE, ordered.to.int = TRUE)

  if (is.null(penalty)) {
    penalty = 9 * sum(data$target[, 2L])
  }

  pars = c(list(
    time = data$target[, 1L],
    status = data$target[, 2L],
    x = as.matrix(fixDataForLearner(data$data, info)),
    penalty = penalty,
    weights = .weights
  ), list(...))
  rm(data)

  res = do.call(CoxBoost::cv.CoxBoost, pars)
  res$optimal.step
  if (res$optimal.step == 0L) {
    warning("Could not determine the optimal step number in cv.CoxBoost")
  }

  pars = insert(pars, list(stepno = res$optimal.step))
  pars$maxstepno = NULL
  attachTrainingInfo(do.call(CoxBoost::CoxBoost, pars), info)
}

#' @export
predictLearner.surv.cv.CoxBoost = function(.learner, .model, .newdata, ...) {
  info = getTrainingInfo(.model)
  .newdata = as.matrix(fixDataForLearner(.newdata, info))
  as.numeric(predict(.model$learner.model, newdata = .newdata, type = "lp"))
}
