#' @export
makeRLearner.classif.gbm = function() {
  makeRLearnerClassif(
    cl = "classif.gbm",
    package = "gbm",
    par.set = makeParamSet(
      makeDiscreteLearnerParam(id = "distribution", default = "bernoulli", values = c("gaussian", "bernoulli", "huberized", "adaboost", "coxph", "pairwise", "laplace", "poisson", "tdist", "quantile", "multinomial")),
      makeIntegerLearnerParam(id = "n.trees", default = 100L, lower = 1L),
      makeIntegerLearnerParam(id = "cv.folds", default = 0L),
      makeIntegerLearnerParam(id = "interaction.depth", default = 1L, lower = 1L),
      makeIntegerLearnerParam(id = "n.minobsinnode", default = 10L, lower = 1L),
      makeNumericLearnerParam(id = "shrinkage", default = 0.1, lower = 0),
      makeNumericLearnerParam(id = "bag.fraction", default = 0.5, lower = 0, upper = 1),
      makeNumericLearnerParam(id = "train.fraction", default = 1, lower = 0, upper = 1),
      makeLogicalLearnerParam(id = "keep.data", default = TRUE, tunable = FALSE),
      makeLogicalLearnerParam(id = "verbose", default = FALSE, tunable = FALSE),
      makeIntegerLearnerParam(id = "n.cores", default = 1, tunable = FALSE)
    ),
    properties = c("twoclass", "multiclass", "missings", "numerics", "factors", "prob", "weights", "featimp"),
    par.vals = list(keep.data = FALSE),
    name = "Gradient Boosting Machine",
    short.name = "gbm",
    note = paste0(collapse = "", c("`keep.data` is set to FALSE to reduce memory requirements.\n",
      "Param 'n.cores' has been to set to '1' by default to suppress parallelization by the package.")),
    callees = "gbm"
  )
}

#' @export
trainLearner.classif.gbm = function(.learner, .task, .subset, .weights = NULL, ...) {
  td = getTaskDesc(.task)
  if (length(td$class.levels) == 2L) {
    d = getTaskData(.task, .subset, recode.target = "01")
  } else {
    d = getTaskData(.task, .subset)
  }
  if (is.null(.weights)) {
    f = getTaskFormula(.task)
    gbm::gbm(f, data = d, ...)
  } else {
    f = getTaskFormula(.task)
    gbm::gbm(f, data = d, weights = .weights, ...)
  }
}

#' @export
predictLearner.classif.gbm = function(.learner, .model, .newdata, ...) {
  td = .model$task.desc
  m = .model$learner.model
  p = gbm::predict.gbm(m, newdata = .newdata, type = "response", n.trees = m$n.trees, single.tree = FALSE, ...)
  if (length(td$class.levels) == 2L) {
    levs = c(td$negative, td$positive)
    if (.learner$predict.type == "prob") {
      y = matrix(0, ncol = 2, nrow = nrow(.newdata))
      colnames(y) = levs
      y[, 1L] = 1 - p
      y[, 2L] = p
      return(y)
    } else {
      p = as.factor(ifelse(p > 0.5, levs[2L], levs[1L]))
      names(p) = NULL
      return(p)
    }
  } else {
    # previously we had `p[, , 1L]`. This results in a numeric when nrow == 1 and
    # triggers an assertion error later. The following always return a matrix
    # see also https://stackoverflow.com/questions/58702027/r-convert-array-to-matrix-with-one-row
    p = array(c(p), dim(p)[-3], dimnames = dimnames(p)[1:2])
    if (.learner$predict.type == "prob") {
      return(p)
    } else {
      ind = getMaxIndexOfRows(p)
      cns = colnames(p)
      return(factor(cns[ind], levels = cns))
    }
  }
}

#' @export
getFeatureImportanceLearner.classif.gbm = function(.learner, .model, ...) {
  mod = getLearnerModel(.model, more.unwrap = TRUE)
  gbm::relative.influence(mod, mod$n.trees, ...)
}
