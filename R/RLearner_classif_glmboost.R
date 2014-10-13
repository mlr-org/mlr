#' @export
makeRLearner.classif.glmboost = function() {
  makeRLearnerClassif(
    cl = "classif.glmboost",
    package = "mboost",
    par.set = makeParamSet(
      makeDiscreteLearnerParam(id = "family", default = mboost::Binomial(), values = list(AdaExp = mboost::AdaExp(), Binomial = mboost::Binomial())),
      makeIntegerLearnerParam(id = "mstop", default = 100L, lower = 1L),
      makeNumericLearnerParam(id = "nu", default = 0.1, lower = 0, upper = 1),
      makeLogicalLearnerParam(id = "center", default = FALSE)
    ),
    par.vals = list(family = mboost::Binomial()),
    properties = c("twoclass", "numerics", "factors", "prob", "weights"),
    name = "Boosting for GLMs",
    short.name = "glmbst",
    note = "`family` has been set to `Binomial()` by default."
  )
}

#' @export
trainLearner.classif.glmboost = function(.learner, .task, .subset, .weights = NULL, mstop, nu, risk, ...) {
  ctrl = learnerArgsToControl(mboost::boost_control, mstop, nu, risk)
  if (is.null(.weights)) {
    f = getTaskFormula(.task)
    mboost::glmboost(f, data = getTaskData(.task, .subset), control = ctrl, , ...)
  } else  {
    f = as.formula(getTaskFormulaAsString(.task))
    mboost::glmboost(f, data = getTaskData(.task, .subset), control = ctrl, weights = .weights, ...)
  }
}

#' @export
predictLearner.classif.glmboost = function(.learner, .model, .newdata, ...) {
  type = ifelse(.learner$predict.type == "response", "class", "response")
  p = predict(.model$learner.model, newdata = .newdata, type = type, ...)
  if (.learner$predict.type  == "prob") {
    p = p[, 1L]
    y = matrix(0, ncol = 2L, nrow = nrow(.newdata))
    colnames(y) = .model$task.desc$class.levels
    y[, 1L] = p
    y[, 2L] = 1-p
    return(y)
  } else {
    return(p)
  }
}
