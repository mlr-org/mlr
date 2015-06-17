#' @export
makeRLearner.regr.lm = function() {
  makeRLearnerRegr(
    cl = "regr.lm",
    package = "stats",
    par.set = makeParamSet(
  	  makeDiscreteLearnerParam(id = "method", default = "moment",
        values = c("moment", "mle", "mve", "t")),
  		makeNumericLearnerParam(id = "nu", lower = 2,
        requires = expression(method == "t")),
      makeNumericLearnerParam(id = "tol", default = 1.0e-4, lower = 0),
      makeLogicalLearnerParam(id = "singular.ok", default = TRUE, tunable = FALSE)
    ),
    properties = c("numerics", "factors", "se", "weights"),
    name = "Simple Linear Regression",
    short.name = "lm",
    note = ""
  )
}

#' @export
trainLearner.regr.lm = function(.learner, .task, .subset, .weights = NULL,  ...) {
  d = getTaskData(.task, .subset)
  if (is.null(.weights)) {
    f = getTaskFormula(.task)
    stats::lm(f, data = d, ...)
  } else  {
    f = as.formula(getTaskFormulaAsString(.task))
    stats::lm(f, data = d, weights = .weights, ...)
  }
}

#' @export
predictLearner.regr.lm = function(.learner, .model, .newdata, ...) {
  if(.learner$predict.type == "response") {
    predict(.model$learner.model, newdata = .newdata, se.fit = FALSE, ...)
  } else {
    p = predict(.model$learner.model, newdata = .newdata, se.fit = TRUE, ...)
    cbind(p$fit, p$se.fit)
  }
}
