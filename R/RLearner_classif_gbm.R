#' @export
makeRLearner.classif.gbm = function() {
  makeRLearnerClassif(
    cl = "classif.gbm",
    package = "gbm",
    par.set = makeParamSet(
      makeDiscreteLearnerParam(id = "distribution", values = c("bernoulli", "adaboost", "gaussian", "laplace", "huberized", "multinomial", "poisson", "pairwise")),
      makeIntegerLearnerParam(id = "n.trees", default = 100L, lower = 1L),
      makeIntegerLearnerParam(id = "interaction.depth", default = 1L, lower = 1L),
      makeIntegerLearnerParam(id = "n.minobsinnode", default = 10L, lower = 1L),
      makeNumericLearnerParam(id = "shrinkage", default = 0.001, lower = 0),
      makeNumericLearnerParam(id = "bag.fraction", default = 0.5, lower = 0, upper = 1),
      makeNumericLearnerParam(id = "train.fraction", default = 1, lower = 0, upper = 1)
    ),
    properties = c("twoclass", "multiclass", "missings", "numerics", "factors", "prob", "weights"),
    name = "Gradient Boosting Machine",
    short.name = "gbm",
    note = ""
  )
}

#' @export
trainLearner.classif.gbm = function(.learner, .task, .subset, .weights = NULL,  ...) {
  if(length(.task$task.desc$class.levels) == 2L)
    d = getTaskData(.task, .subset, recode.target = "01")
  else
    d = getTaskData(.task, .subset)
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
  p = predict(m, newdata = .newdata, type = "response", n.trees = m$n.trees, single.tree = FALSE, ...)
  if (length(.model$task.desc$class.levels) == 2) {
    levs = c(.model$task.desc$negative, .model$task.desc$positive)
    if (.learner$predict.type == "prob") {
      y = matrix(0, ncol = 2, nrow = nrow(.newdata))
      colnames(y) = levs
      y[, 1L] = 1-p
      y[, 2L] = p
      return(y)
    } else {
      p = as.factor(ifelse(p > 0.5, levs[2L], levs[1L]))
      names(p) = NULL
      return(p)
    }
  } else {
    p = p[,,1L]
    if (.learner$predict.type == "prob") {
      return(p)
    } else {
      ind = getMaxIndexOfRows(p)
      cns = colnames(p)
      return(factor(cns[ind], levels = cns))
    }
  }
}
