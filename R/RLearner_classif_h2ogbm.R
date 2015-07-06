#' @export
makeRLearner.classif.h2ogbm = function() {
  makeRLearnerClassif(
    cl = "classif.h2ogbm",
    package = "h2o",
    par.set = makeParamSet(
      makeIntegerLearnerParam("ntrees", lower = 1L, default = 50L),
      makeIntegerLearnerParam("max_depth", lower = 1L, default = 5L),
      makeIntegerLearnerParam("min_rows", lower = 1L,  default = 10L),
      makeNumericLearnerParam("learn_rate", lower = 0, upper = 1, default = 0.1),
      makeIntegerLearnerParam("nbins", lower = 1L, default = 20L),
      makeIntegerLearnerParam("nbins_cats", lower = 1L, default = 1024L),
      makeLogicalLearnerParam("balance_classes", default = FALSE),
      makeIntegerLearnerParam("max_after_balance_size", lower = 0, default = 1L),
      makeIntegerLearnerParam("seed", tunable = FALSE)
    ),
    properties = c("twoclass", "multiclass", "numerics", "factors", "prob"),
    name = "h2o.gbm",
    short.name = "h2o.gbm",
    note = "'distribution' is set automatically by mlr."
  )
}

#' @export
trainLearner.classif.h2ogbm = function(.learner, .task, .subset, .weights = NULL,  ...) {
  y = getTaskTargetNames(.task)
  x = getTaskFeatureNames(.task)
  d = getTaskData(.task, subset = .subset)
  h2of = as.h2o(d)
  distribution = ifelse(length(getTaskDescription(.task)$class.levels) == 2L, "bernoulli", "multinomial")
  h2o.gbm(y = y, x = x, training_frame = h2of, distribution = distribution, ...)
}

#' @export
predictLearner.classif.h2ogbm = function(.learner, .model, .newdata, ...) {
  m = .model$learner.model
  h2of = as.h2o(.newdata)
  p = h2o::h2o.predict(m, newdata = h2of, ...)
  p.df = as.data.frame(p)
  if (.learner$predict.type == "response") {
    return(p.df$predict)
  } else {
    p.df$predict = NULL
    return(as.matrix(p.df))
  }
}

