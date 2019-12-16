#' @export
makeRLearner.regr.h2o.randomForest = function() {
  makeRLearnerRegr(
    cl = "regr.h2o.randomForest",
    package = "h2o",
    par.set = makeParamSet(
      makeIntegerLearnerParam("mtries", lower = -1L, default = -1L),
      makeNumericLearnerParam("sample_rate", lower = 0, upper = 1, default = 0.632),
      makeLogicalLearnerParam("build_tree_one_node", default = FALSE, tunable = FALSE),
      makeIntegerLearnerParam("ntrees", lower = 1L, default = 50L),
      makeIntegerLearnerParam("max_depth", lower = 1L, default = 20L),
      makeIntegerLearnerParam("min_rows", lower = 1L, default = 1L),
      makeIntegerLearnerParam("nbins", lower = 1L, default = 20L),
      makeIntegerLearnerParam("nbins_cats", lower = 1L, default = 1024L),
      makeIntegerLearnerParam("seed", tunable = FALSE)
    ),
    properties = c("numerics", "factors", "missings"),
    name = "h2o.randomForest",
    short.name = "h2o.rf",
    callees = "h2o.randomForest"
  )
}

#' @export
trainLearner.regr.h2o.randomForest = function(.learner, .task, .subset, .weights = NULL, ...) {
  # check if h2o connection already exists, otherwise start one
  conn.up = tryCatch(h2o::h2o.getConnection(), error = function(err) {
    return(FALSE)
  })
  if (!inherits(conn.up, "H2OConnection")) {
    h2o::h2o.init()
  }
  x = getTaskFeatureNames(.task)
  y = getTaskTargetNames(.task)
  d = getTaskData(.task, subset = .subset)
  h2of = h2o::as.h2o(d)
  h2o::h2o.randomForest(x = x, y = y, training_frame = h2of, ...)
}

#' @export
predictLearner.regr.h2o.randomForest = function(.learner, .model, .newdata, ...) {
  m = .model$learner.model
  h2of = h2o::as.h2o(.newdata)
  p = h2o::h2o.predict(m, newdata = h2of, ...)
  p.df = as.data.frame(p)
  return(p.df$predict)
}
