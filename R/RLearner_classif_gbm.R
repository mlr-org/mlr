#' @export
makeRLearner.classif.gbm = function() {
  makeRLearnerClassif(
    cl = "classif.gbm",
    package = "gbm",
    par.set = makeParamSet(
      makeDiscreteLearnerParam(id = "distribution", default = "bernoulli", values = c("bernoulli", "adaboost")),
      makeIntegerLearnerParam(id = "n.trees", default = 100L, lower = 1L),
      makeIntegerLearnerParam(id = "interaction.depth", default = 1L, lower = 1L),
      makeIntegerLearnerParam(id = "n.minobsinnode", default = 10L, lower = 1L),
      makeNumericLearnerParam(id = "shrinkage", default = 0.001, lower = 0),
      makeNumericLearnerParam(id = "bag.fraction", default = 0.5, lower = 0, upper = 1),
      makeNumericLearnerParam(id = "train.fraction", default = 1, lower = 0, upper = 1)
    ),
    par.vals = list(distribution = "bernoulli"),
    properties = c("twoclass", "missings", "numerics", "factors", "prob", "weights")
  )
}

#' @export
trainLearner.classif.gbm = function(.learner, .task, .subset, .weights = NULL,  ...) {
  d = getTaskData(.task, .subset, recode.target = "01")
  if (is.null(.weights)) {
    f = getTaskFormula(.task)
    gbm(f, data = d, keep.data = FALSE, verbose = FALSE, ...)
  } else  {
    f = as.formula(getTaskFormulaAsString(.task))
    gbm(f, data = d, keep.data = FALSE, verbose = FALSE, weights = .weights, ...)
  }
}

#' @export
predictLearner.classif.gbm = function(.learner, .model, .newdata, ...) {
  m = .model$learner.model
  p = predict(m, newdata = .newdata, type = "response", n.trees = length(m$trees), single.tree = FALSE, ...)
  levs = c(.model$task.desc$negative, .model$task.desc$positive)
  if (.learner$predict.type == "prob") {
    y = matrix(0, ncol = 2, nrow = nrow(.newdata))
    colnames(y) = levs
    y[,1L] = 1-p
    y[,2L] = p
    return(y)
  } else {
    p = as.factor(ifelse(p > 0.5, levs[2L], levs[1L]))
    names(p) = NULL
    return(p)
  }
}
