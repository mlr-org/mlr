#' @export
makeRLearner.surv.optimCoxBoostPenalty = function() {
  makeRLearnerSurv(
    cl = "surv.optimCoxBoostPenalty",
    package = "!CoxBoost",
    par.set = makeParamSet(
      makeIntegerLearnerParam(id = "minstepno", default = 50L, lower = 0L),
      makeIntegerLearnerParam(id = "maxstepno", default = 100L, lower = 0L),
      makeIntegerLearnerParam(id = "K", default = 10L, lower = 1L),
      makeDiscreteLearnerParam(id = "type", default = "verweij",
        values = c("verweij", "naive")),
      makeLogicalLearnerParam(id = "standardize", default = TRUE),
      makeDiscreteLearnerParam(id = "criterion", default = "pscore",
        values = c("pscore", "score", "hpscore", "hscore")),
      makeNumericLearnerParam(id = "stepsize.factor", default = 1, lower = 0),
      makeDiscreteLearnerParam(id = "sf.scheme", default = "sigmoid",
        values = c("sigmoid", "linear")),
      makeIntegerLearnerParam(id = "iter.max", default = 10L, lower = 1L),
      makeNumericLearnerParam(id = "upper.margin", default = 0.05, lower = 0, upper = 1),
      makeLogicalLearnerParam(id = "return.score", default = TRUE, tunable = FALSE),
      makeLogicalLearnerParam(id = "parallel", default = FALSE, tunable = FALSE),
      makeLogicalLearnerParam(id = "upload.x", default = FALSE, tunable = FALSE),
      makeLogicalLearnerParam(id = "multicore", default = FALSE, tunable = FALSE),
      makeLogicalLearnerParam(id = "trace", default = FALSE, tunable = FALSE)
      # FIXME: still missing some arguments
    ),
    par.vals = list(return.score = FALSE),
    properties = c("numerics", "factors", "weights", "rcens"),
    name = "Cox Proportional Hazards Model with Componentwise Likelihood based Boosting, automatic tuning enabled",
    short.name = "optimCoxBoostPenalty",
    note = "Factors automatically get converted to dummy columns, ordered factors to integer."
  )
}

#' @export
trainLearner.surv.optimCoxBoostPenalty = function(.learner, .task, .subset, .weights = NULL, return.score, ...) {
  data = getTaskData(.task, subset = .subset, target.extra = TRUE, recode.target = "rcens")
  info = getFixDataInfo(data$data, factors.to.dummies = TRUE, ordered.to.int = TRUE)

  pars = c(list(
    time = data$target[, 1L],
    status = data$target[, 2L],
    x = as.matrix(fixDataForLearner(data$data, info)),
    weights = .weights
  ), list(...))
  rm(data)

  cb = do.call(CoxBoost::optimCoxBoostPenalty, pars)
  if (cb$cv.res$optimal.step == 0L)
    stop("Error modeling CoxBoost: Could not determine the optimal step number")

  # remove iter max which may not be passed down to CoxBoost directly
  pars$iter.max = NULL

  pars = insert(pars, list(stepno = cb$cv.res$optimal.step, penalty = cb$penalty, return.score = return.score))
  attachTrainingInfo(do.call(CoxBoost::CoxBoost, pars), info)
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
