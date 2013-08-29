#' @S3method makeRLearner regr.nnet
makeRLearner.regr.nnet = function() {
  makeRLearnerRegr(
    cl = "regr.nnet",
    package = "nnet",
    par.set = makeParamSet(
      makeIntegerLearnerParam(id="size", default=3L, lower=0),
      makeIntegerLearnerParam(id="maxit", default=100L, lower=1L),
      makeNumericLearnerParam(id="decay", default=0, lower=0)
    ),
    par.vals = list(size=3L),
    missings = FALSE,
    numerics = TRUE,
    factors = TRUE,
    se = FALSE,
    weights = TRUE
  )
}

#' @S3method trainLearner regr.nnet
trainLearner.regr.nnet = function(.learner, .task, .subset, .weights,  ...) {
  if (missing(.weights)) {
    f = getTaskFormula(.task)
    nnet(f, data=getTaskData(.task, .subset), linout=TRUE, ...)
  } else  {
    f = as.formula(getTaskFormulaAsString(.task))
    nnet(f, data=getTaskData(.task, .subset), linout=TRUE, weights=.weights, ...)
  }
}

#' @S3method predictLearner regr.nnet
predictLearner.regr.nnet = function(.learner, .model, .newdata, ...) {
  predict(.model$learner.model, newdata=.newdata, ...)[,1L]
}
