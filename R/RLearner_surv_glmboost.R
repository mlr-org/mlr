#' @export
makeRLearner.surv.glmboost = function() {
  makeRLearnerSurv(
    cl = "surv.glmboost",
    package = c("!survival", "mboost"),
    par.set = makeParamSet(
      makeDiscreteLearnerParam(id = "family", default = mboost::CoxPH(), values = list(CoxPH = mboost::CoxPH(), Weibull = mboost::Weibull(), Loglog = mboost::Loglog(), Lognormal = mboost::Lognormal())),
      makeIntegerLearnerParam(id = "mstop", default = 100L, lower = 1L),
      makeNumericLearnerParam(id = "nu", default = 0.1, lower = 0, upper = 1),
      makeLogicalLearnerParam(id = "center", default = FALSE),
      makeDiscreteLearnerParam(id = "m", default = "mstop", values = c("mstop", "cv")),
      makeLogicalLearnerParam(id = "use.formula", default = TRUE, when = "both")
    ),
    par.vals = list(
      family = mboost::CoxPH(),
      m = "mstop",
      use.formula = TRUE
    ),
    properties = c("numerics", "factors", "ordered", "weights", "rcens"),
    name = "Gradient Boosting with Componentwise Linear Models",
    short.name = "glmboost",
    note = paste(
      "`family` has been set to `CoxPH()` by default.",
      "Maximum number of boosting iterations is set via `mstop`, the actual number used for prediction is controlled by `m`."
    )
  )
}

#' @export
trainLearner.surv.glmboost = function(.learner, .task, .subset, .weights = NULL, family, mstop, nu, m, use.formula, ...) {
  envir = loadNamespace("mboost")
  ctrl = learnerArgsToControl(mboost::boost_control, mstop, nu)
  if (use.formula) {
    f = getTaskFormula(.task)
    model = if (is.null(.weights)) {
      mboost::glmboost(f, data = getTaskData(.task, subset = .subset, recode.target = "rcens"), control = ctrl, family = family, ...)
    } else  {
      mboost::glmboost(f, data = getTaskData(.task, subset = .subset, recode.target = "rcens"), control = ctrl, weights = .weights, family = family, ...)
    }
  } else {
    data = getTaskData(.task, subset = .subset, target.extra = TRUE, recode.target = "rcens")
    info = getFixDataInfo(data$data, factors.to.dummies = TRUE, ordered.to.int = TRUE)
    data$data = as.matrix(fixDataForLearner(data$data, info))
    model = if (is.null(.weights)) {
      mboost::glmboost(x = data$data, y = data$target, control = ctrl, family = family, ...)
    } else {
      mboost::glmboost(x = data$data, y = data$target, control = ctrl, weights = .weights, family = family, ...)
    }
    model = attachTrainingInfo(model, info)
  }

  if (m == "cv") {
    mboost::mstop(model) = mboost::mstop(mboost::cvrisk(model, papply = lapply))
  }
  model
}

#' @export
predictLearner.surv.glmboost = function(.learner, .model, .newdata, use.formula, ...) {
  if (!use.formula) {
    info = getTrainingInfo(.model)
    .newdata = as.matrix(fixDataForLearner(.newdata, info))
  }
  if(.learner$predict.type == "response")
    predict(.model$learner.model, newdata = .newdata, type = "link")
  else
    stop("Unknown predict type")
}
