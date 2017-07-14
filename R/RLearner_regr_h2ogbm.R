#' @export
makeRLearner.regr.h2o.gbm = function() {
  makeRLearnerRegr(
    cl = "regr.h2o.gbm",
    package = "h2o",
    par.set = makeParamSet(
      makeIntegerLearnerParam("ntrees", lower = 1L, default = 50L),
      makeIntegerLearnerParam("max_depth", lower = 1L, default = 5L),
      makeIntegerLearnerParam("min_rows", lower = 1L,  default = 10L),
      makeNumericLearnerParam("learn_rate", lower = 0, upper = 1, default = 0.1),
      makeIntegerLearnerParam("nbins", lower = 1L, default = 20L),
      makeIntegerLearnerParam("nbins_cats", lower = 1L, default = 1024),
      makeIntegerLearnerParam("seed", tunable = FALSE)
    ),
    properties = c("numerics", "factors", "missings"),
    name = "h2o.gbm",
    short.name = "h2o.gbm",
    note = "'distribution' is set automatically to 'gaussian'.",
    callees = "h2o.gbm"
  )
}

#' @export
trainLearner.regr.h2o.gbm = function(.learner, .task, .subset, .weights = NULL,  ...) {
  # check if h2o connection already exists, otherwise start one
  conn.up = tryCatch(h2o::h2o.getConnection(), error = function(err) return(FALSE))
  if (!inherits(conn.up, "H2OConnection")) {
    h2o::h2o.init()
  }
  y = getTaskTargetNames(.task)
  x = getTaskFeatureNames(.task)
  d = getTaskData(.task, subset = .subset)
  h2of = h2o::as.h2o(d)
  h2o::h2o.gbm(y = y, x = x, training_frame = h2of, distribution = "gaussian", ...)
}

#' @export
predictLearner.regr.h2o.gbm = function(.learner, .model, .newdata, ...) {
  m = .model$learner.model
  h2of = h2o::as.h2o(.newdata)
  p = h2o::h2o.predict(m, newdata = h2of, ...)
  p.df = as.data.frame(p)
  return(p.df$predict)
}
