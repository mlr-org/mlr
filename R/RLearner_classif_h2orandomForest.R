#' @export
makeRLearner.classif.h2o.randomForest = function() {
  makeRLearnerClassif(
    cl = "classif.h2o.randomForest",
    package = "h2o",
    par.set = makeParamSet(
      makeIntegerLearnerParam("mtries", lower = -1L, default = -1L),
      makeNumericLearnerParam("sample_rate", lower = 0, upper = 1, default = 0.632),
      makeLogicalLearnerParam("build_tree_one_node", default = FALSE, tunable = FALSE),
      makeIntegerLearnerParam("ntrees", lower = 1L, default = 50L),
      makeIntegerLearnerParam("max_depth", lower = 1L, default = 20L),
      makeIntegerLearnerParam("min_rows", lower = 1L,  default = 1L),
      makeIntegerLearnerParam("nbins", lower = 1L, default = 20L),
      makeIntegerLearnerParam("nbins_cats", lower = 1L, default = 1024L),
      makeLogicalLearnerParam("binomial_double_trees", default = TRUE),
      makeLogicalLearnerParam("balance_classes", default = FALSE),
      makeIntegerLearnerParam("max_after_balance_size", lower = 0L, default = 5L),
      makeIntegerLearnerParam("seed", tunable = FALSE)
    ),
    properties = c("twoclass", "multiclass", "numerics", "factors", "missings", "prob"),
    name = "h2o.randomForest",
    short.name = "h2o.rf",
    callees = "h2o.randomForest"
  )
}

#' @export
trainLearner.classif.h2o.randomForest = function(.learner, .task, .subset, .weights = NULL,  ...) {
  # check if h2o connection already exists, otherwise start one
  conn.up = tryCatch(h2o::h2o.getConnection(), error = function(err) return(FALSE))
  if (!inherits(conn.up, "H2OConnection")) {
    h2o::h2o.init()
  }
  y = getTaskTargetNames(.task)
  x = getTaskFeatureNames(.task)
  d = getTaskData(.task, subset = .subset)
  h2of = h2o::as.h2o(d)
  h2o::h2o.randomForest(y = y, x = x, training_frame = h2of, ...)
}

#' @export
predictLearner.classif.h2o.randomForest = function(.learner, .model, .newdata, ...) {
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
    return(as.matrix(p.df))
  }
}
