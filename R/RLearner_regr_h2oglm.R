#' @export
makeRLearner.regr.h2o.glm = function() {
  makeRLearnerRegr(
    cl = "regr.h2o.glm",
    package = "h2o",
    par.set = makeParamSet(
      makeIntegerLearnerParam("max_iterations", lower = 0L, default = 50L),
      makeNumericLearnerParam("beta_epsilon", lower = 0, default = 0),
      makeLogicalLearnerParam("standardize", default = TRUE),
      makeDiscreteLearnerParam("solver", values = c("IRLSM", "L_BFGS"), default = "IRLSM"),
      makeDiscreteLearnerParam("link", values = c("identity", "log", "inverse"), default = "identity"),
      makeNumericLearnerParam("alpha", lower = 0, upper = 1, default = 0.5),
      makeNumericLearnerParam("lambda", lower = 0, default = 1e-5),
      makeLogicalLearnerParam("lambda_search", default = FALSE),
      makeIntegerLearnerParam("nlambdas", lower = 1L,
        requires = quote(lambda_search == TRUE)),
      makeNumericLearnerParam("lambda_min_ratio", lower = 0, upper = 1, # data dep default
        requires = quote(lambda_search == TRUE)),
      makeUntypedLearnerParam("beta_constraints"),
      makeLogicalLearnerParam("intercept", default = TRUE)
    ),
    properties = c("numerics", "factors", "weights", "missings"),
    name = "h2o.glm",
    short.name = "h2o.glm",
    note = '`family` is always set to `"gaussian"`. The default value of `missing_values_handling` is `"MeanImputation"`, so missing values are automatically mean-imputed.',
    callees = "h2o.glm"
  )
}

#' @export
trainLearner.regr.h2o.glm = function(.learner, .task, .subset, .weights = NULL, ...) {

  # check if h2o connection already exists, otherwise start one
  conn.up = tryCatch(h2o::h2o.getConnection(), error = function(err) {
    return(FALSE)
  })
  if (!inherits(conn.up, "H2OConnection")) {
    h2o::h2o.init()
  }
  y = getTaskTargetNames(.task)
  x = getTaskFeatureNames(.task)
  d = getTaskData(.task, subset = .subset)
  wcol = NULL
  if (!is.null(.weights)) {
    d$.mlr.weights = .weights
    wcol = ".mlr.weights"
  }
  h2of = h2o::as.h2o(d)
  h2o::h2o.glm(y = y, x = x, training_frame = h2of, family = "gaussian", weights_column = wcol, ...)
}

#' @export
predictLearner.regr.h2o.glm = function(.learner, .model, .newdata, ...) {
  m = .model$learner.model
  h2of = h2o::as.h2o(.newdata)
  p = h2o::h2o.predict(m, newdata = h2of, ...)
  p.df = as.data.frame(p)
  return(p.df$predict)
}
