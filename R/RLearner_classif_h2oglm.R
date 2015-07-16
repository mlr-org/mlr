#' @export
makeRLearner.classif.h2oglm = function() {
  makeRLearnerClassif(
    cl = "classif.h2oglm",
    package = "h2o",
    par.set = makeParamSet(
      makeIntegerLearnerParam("max_iterations", lower = 0L, default = 50L),
      makeNumericLearnerParam("beta_epsilon", default = 0),
      makeLogicalLearnerParam("standardize", default = TRUE),
      makeDiscreteLearnerParam("solver", values = c("IRLSM", "L_BFGS"), default = "IRLSM"),
      makeDiscreteLearnerParam("link", values = c("logit", "log"), default = "logit"),
      makeNumericLearnerParam("alpha", lower = 0, upper = 1, default = 0.5),
      makeNumericLearnerParam("prior", lower = 0), # data dependent default
      makeNumericLearnerParam("lambda", lower = 0, default = 1e-5),
      makeLogicalLearnerParam("lambda_search", default = FALSE),
      makeIntegerLearnerParam("nlambdas", lower = 1L,
        requires = expression(lambda_search == TRUE)),
      makeNumericLearnerParam("lambda_min_ratio", lower = 0, upper = 1, # data dep default
        requires = expression(lambda_search == TRUE)),
      makeUntypedLearnerParam("beta_constraints"),
      makeLogicalLearnerParam("intercept", default = TRUE)
    ),
    properties = c("twoclass", "numerics", "factors", "prob", "weights"),
    name = "h2o.glm",
    short.name = "h2o.glm",
    note = "'family' is always set to 'binomial' to get a binary classifier."
  )
}

#' @export
trainLearner.classif.h2oglm = function(.learner, .task, .subset, .weights = NULL,  ...) {
  y = getTaskTargetNames(.task)
  x = getTaskFeatureNames(.task)
  d = getTaskData(.task, subset = .subset)
  h2of = as.h2o(d)
  h2o.glm(y = y, x = x, training_frame = h2of, family = "binomial", ...)
}

#' @export
predictLearner.classif.h2oglm = function(.learner, .model, .newdata, ...) {
  m = .model$learner.model
  h2of = as.h2o(.newdata)
  p = h2o::h2o.predict(m, newdata = h2of, ...)
  p.df = as.data.frame(p)
  
  #FIXME: h2o.glm returns wrong predictions? p0 := positive class???
  colname = c("predict" = "predict", 
    "p0" = .model$task.desc$positive,
    "p1" = .model$task.desc$negative)
  colnames(p.df) = colname[colnames(p.df)]
  p.df$predict = as.factor(as.vector(colname[-1][apply(p.df[,-1], 1, which.max)]))
  
  if (.learner$predict.type == "response") {
    return(p.df$predict)
  } else {
    p.df$predict = NULL
    ret = as.matrix(p.df)
    return(ret)
  }
}

