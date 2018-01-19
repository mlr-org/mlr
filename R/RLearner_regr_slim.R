#' @export
makeRLearner.regr.slim = function() {
  makeRLearnerRegr(
    cl = "regr.slim",
    package = "flare",
    par.set = makeParamSet(
      makeNumericVectorLearnerParam(id = "lambda"),
      makeIntegerLearnerParam(id = "nlambda", default = 5L, lower = 1L),
      makeNumericLearnerParam(id = "lambda.min.value", lower = 0, upper = 1),
      makeNumericLearnerParam(id = "lambda.min.ratio", lower = 0, upper = 1),
      makeNumericLearnerParam(id = "rho", default = 1, lower = 0),
      makeDiscreteLearnerParam(id = "method", values = c("lq", "dantzig", "lasso"), default = "lq"),
      makeNumericLearnerParam(id = "q", lower = 1, upper = 2, requires = quote(method == "lq")),
      makeLogicalLearnerParam(id = "res.sd", default = FALSE),
      makeNumericLearnerParam(id = "prec", default = 1e-5, lower = .Machine$double.eps),
      makeIntegerLearnerParam(id = "max.ite", default = 1e5L),
      makeLogicalLearnerParam(id = "verbose", default = FALSE, tunable = FALSE),
      makeIntegerLearnerParam(id = "lambda.idx", default = 3L, when = "predict")
      # FIXME the default for lambda.id in predict.slim is c(1:3)
    ),
    par.vals = list(lambda.idx = 3L),
    properties = "numerics",
    name = "Sparse Linear Regression using Nonsmooth Loss Functions and L1 Regularization",
    short.name = "slim",
    note = "`lambda.idx` has been set to `3` by default.",
    callees = c("slim", "predict.slim")
  )
}

#' @export
trainLearner.regr.slim = function(.learner, .task, .subset, .weights = NULL, ...) {
  d = getTaskData(.task, .subset, target.extra = TRUE)
  flare::slim(X = as.matrix(d$data), Y = d$target, ...)
}

#' @export
predictLearner.regr.slim = function(.learner, .model, .newdata, ...) {
  predict(.model$learner.model, newdata = as.matrix(.newdata), ...)[[1]][, 1L]
}
