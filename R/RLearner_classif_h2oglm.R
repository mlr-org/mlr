#' @export
makeRLearner.classif.h2o.glm = function() {
  makeRLearnerClassif(
    cl = "classif.h2o.glm",
    package = "h2o",
    par.set = makeParamSet(
      makeIntegerLearnerParam("max_iterations", lower = 0L, default = 50L),
      makeNumericLearnerParam("beta_epsilon", lower = 0, default = 0),
      makeLogicalLearnerParam("standardize", default = TRUE),
      makeDiscreteLearnerParam("solver", values = c("IRLSM", "L_BFGS"), default = "IRLSM"),
      makeDiscreteLearnerParam("link", values = c("logit", "log"), default = "logit"),
      makeNumericLearnerParam("alpha", lower = 0, upper = 1, default = 0.5),
      makeNumericLearnerParam("prior", lower = 0), # data dependent default
      makeNumericLearnerParam("lambda", lower = 0, default = 1e-5),
      makeLogicalLearnerParam("lambda_search", default = FALSE),
      makeIntegerLearnerParam("nlambdas", lower = 1L,
        requires = quote(lambda_search == TRUE)),
      # FIXME: data dep default
      makeNumericLearnerParam("lambda_min_ratio", lower = 0, upper = 1,
        requires = quote(lambda_search == TRUE)),
      makeUntypedLearnerParam("beta_constraints"),
      makeLogicalLearnerParam("intercept", default = TRUE)
    ),
    properties = c("twoclass", "numerics", "factors", "prob", "weights"),
    name = "h2o.glm",
    short.name = "h2o.glm",
    note = "'family' is always set to 'binomial' to get a binary classifier.",
    callees = "h2o.glm"
  )
}

#' @export
trainLearner.classif.h2o.glm = function(.learner, .task, .subset, .weights = NULL,  ...) {
  # check if h2o connection already exists, otherwise start one
  conn.up = tryCatch(h2o::h2o.getConnection(), error = function(err) return(FALSE))
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
  h2o::h2o.glm(y = y, x = x, training_frame = h2of, family = "binomial", weights_column = wcol, ...)
}

#' @export
predictLearner.classif.h2o.glm = function(.learner, .model, .newdata, ...) {
  m = .model$learner.model
  h2of = h2o::as.h2o(.newdata)
  p = h2o::h2o.predict(m, newdata = h2of, ...)
  p.df = as.data.frame(p)

  # check if class names are integers. if yes, colnames of p.df need to be adapted
  int = stri_detect_regex(p.df$predict, "^[[:digit:]]+$")
  pcol = stri_detect_regex(colnames(p.df), "^p[[:digit:]]+$")
  if (any(int) && any(pcol))
    colnames(p.df)[pcol] = stri_sub(colnames(p.df)[pcol], 2L)

  if (.learner$predict.type == "response") {
    return(p.df$predict)
  } else {
    p.df$predict = NULL
    ret = as.matrix(p.df)
    return(ret)
  }
}
