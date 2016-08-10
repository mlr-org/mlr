#' @export
makeRLearner.classif.parallelForest = function() {
  makeRLearnerClassif(
    cl = "classif.parallelForest",
    package = "!ParallelForest",
    par.set = makeParamSet(
      ##For all the parameters here package manual states:
      ##If not provided as input, the package will attempt to choose a reasonable value
      makeIntegerLearnerParam(id = "min_node_obs", lower = 1L),
      makeIntegerLearnerParam(id = "max_depth", lower = 1L),
      makeIntegerLearnerParam(id = "numsamps", lower = 1L),
      makeIntegerLearnerParam(id = "numvars", lower = 1L),
      makeIntegerLearnerParam(id = "numboots", lower = 1L)
    ),
    properties = c("twoclass", "numerics"),
    name = "Parallel Random Forest",
    short.name = "parallelForest"
    )
}

#' @export
trainLearner.classif.parallelForest = function(.learner, .task, .subset, .weights = NULL, ...) {
  f = getTaskFormula(.task)
  df = getTaskData(.task, .subset)
  ParallelForest::grow.forest(formula = f, data = df, na.action = na.omit, 
    impurity.function = "gini", model = FALSE, x = FALSE, y = FALSE, ...)
}

#' @export
predictLearner.classif.parallelForest = function(.learner, .model, .newdata, ...) {
  predict(.model$learner.model, newdata = .newdata, ...)
}