#' @export
makeRLearner.classif.rda = function() {
  makeRLearnerClassif(
    cl = "classif.rda",
    package = "klaR",
    par.set = makeParamSet(
      makeNumericLearnerParam(id = "lambda", lower = 0, upper = 1),
      makeNumericLearnerParam(id = "gamma", lower = 0, upper = 1),
      makeLogicalLearnerParam(id = "crossval", default = TRUE),
      makeIntegerLearnerParam(id = "fold", default = 10L, lower = 1L),
      makeNumericLearnerParam(id = "train.fraction", default = 0.5, lower = 0, upper = 1),
      makeLogicalLearnerParam(id = "output", default = FALSE, tunable = FALSE),
      makeDiscreteLearnerParam(id = "schedule", default = 2L, values = 1:2, requires = quote(simAnn == TRUE)),
      makeNumericLearnerParam(id = "T.start", default = 0.1, lower = 0, requires = quote(simAnn == TRUE)),
      makeNumericLearnerParam(id = "halflife", default = 50, lower = 0, requires = quote(simAnn == TRUE && schedule == 1)),
      makeNumericLearnerParam(id = "zero.temp", default = 0.01, lower = 0, requires = quote(simAnn == TRUE && schedule == 1)),
      makeNumericLearnerParam(id = "alpha", default = 2, lower = 1, requires = quote(simAnn == TRUE && schedule == 2)),
      makeIntegerLearnerParam(id = "K", default = 100L, lower = 1L, requires = quote(simAnn == TRUE && schedule == 2)),
      makeLogicalLearnerParam(id = "trafo", default = TRUE),
      makeLogicalLearnerParam(id = "simAnn", default = FALSE),
      makeLogicalLearnerParam(id = "estimate.error", default = TRUE)
    ),
    par.vals = list(estimate.error = FALSE),
    properties = c("twoclass", "multiclass", "numerics", "factors", "prob"),
    name = "Regularized Discriminant Analysis",
    short.name = "rda",
    note = "`estimate.error` has been set to `FALSE` by default for speed.",
    callees = "rda"
  )
}

#' @export
trainLearner.classif.rda = function(.learner, .task, .subset, .weights = NULL, ...) {
  f = getTaskFormula(.task)
  klaR::rda(f, data = getTaskData(.task, .subset), ...)
}

#' @export
predictLearner.classif.rda = function(.learner, .model, .newdata, ...) {
  p = predict(.model$learner.model, newdata = .newdata, ...)
  if (.learner$predict.type == "response") {
    return(p$class)
  }
  return(p$posterior)
}
