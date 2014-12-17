#' @export
makeRLearner.regr.crs = function() {
  makeRLearnerRegr(
    cl = "regr.crs",
    package = "crs",
    par.set = makeParamSet(
      makeIntegerVectorLearnerParam(id = "degree", default = 3, lower = 0),
      makeIntegerVectorLearnerParam(id = "segments", default = 1, lower = 1),
      makeIntegerVectorLearnerParam(id = "lambda"),
      makeLogicalLearnerParam(id = "lambda.discrete", default = FALSE),
      makeIntegerLearnerParam(id = "lambda.discrete.num", default = 100, lower = 0,
        requires = expression(lambda.discret)),
      makeDiscreteLearnerParam(id = "cv", default = "nomad",
        values = c("nomad", "exhaustive", "none")),
      makeIntegerLearnerParam(id = "cv.treshold", default = 10000, lower = 0),
      makeDiscreteLearnerParam(id = "cv.func", default = "cv.ls",
        values = c("cv.ls", "cv.gcv", "cv.aic")),
      makeLogicalLearnerParam(id = "kernel", default = TRUE),
      makeIntegerLearnerParam(id = "degree.max", default = 10, lower = 0),
      makeIntegerLearnerParam(id = "segments.max", default = 10, lower = 1),
      makeIntegerLearnerParam(id = "degree.min", default = 0, lower = 0),
      makeIntegerLearnerParam(id = "segments.min", default = 1, lower = 1),
      makeIntegerLearnerParam(id = "cv.df.min", default = 1,
        requires = expression(cv=="nomad")),
      makeDiscreteLearnerParam(id = "complexity", default = "degree-knots",
        values = c("degree-knots", "degree", "knots")),
      makeDiscreteLearnerParam(id = "knots", default = "quantiles",
        values = c("quantiles", "uniform", "auto")),
      makeDiscreteLearnerParam(id = "basis", default = "auto",
        values = c("auto", "additive", "tensor", "glp")),
      makeLogicalLearnerParam(id = "prune", default = FALSE),
      makeIntegerLearnerParam(id = "restarts", default = 0, lower = 0),
      makeIntegerLearnerParam(id = "nmulti", default = 5, lower = 0),
      makeLogicalLearnerParam(id = "singular.ok", default = FALSE)

    ),
    par.vals = list(),
    properties = c("numerics", "factors", "se", "weights"),
    name = "Regression Splines",
    short.name = "crs",
    note = ""
  )
}

#' @export
trainLearner.regr.crs = function(.learner, .task, .subset, .weights = NULL,  ...) {
  f = getTaskFormula(.task)
  # FIXME: see reported issue https://github.com/JeffreyRacine/R-Package-crs/issues/2
  requirePackages("crs", namespace.only = FALSE)
  if (is.null(.weights)) {
    crs::crs(formula = f, data = getTaskData(.task, .subset), ...)
  } else  {
    crs::crs(formula = f, data = getTaskData(.task, .subset), weights = .weights, ...)
  }
}

#' @export
predictLearner.regr.crs = function(.learner, .model, .newdata, ...) {
  if (.learner$predict.type == "se") {
    pred = predict(.model$learner.model, newdata = .newdata, ...)
    lwr = attr(pred, "lwr")
    attr(pred, "lwr") = NULL
    attr(pred, "upr") = NULL
    # FIXME: make sure that this is correct, ask Daniel
    se = (pred - lwr) * sqrt(.model$task.desc$size) / qnorm(0.95)
    cbind(pred, se)
  } else {
    pred = predict(.model$learner.model, newdata = .newdata, ...)
    attr(pred, "lwr") = NULL
    attr(pred, "upr") = NULL
    pred
  }
}
