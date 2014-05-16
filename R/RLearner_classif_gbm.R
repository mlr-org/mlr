#' @S3method makeRLearner classif.gbm
makeRLearner.classif.gbm = function() {
  makeRLearnerClassif(
    cl = "classif.gbm",
    package = "gbm",
    par.set = makeParamSet(
      makeDiscreteLearnerParam(id="distribution", default="bernoulli", values=c("bernoulli", "adaboost")),
      makeIntegerLearnerParam(id="n.trees", default=100L, lower=1L),
      makeIntegerLearnerParam(id="interaction.depth", default=1L, lower=1L),
      makeIntegerLearnerParam(id="n.minobsinnode", default=10L, lower=1L),
      makeNumericLearnerParam(id="shrinkage", default=0.001, lower=0),
      makeNumericLearnerParam(id="bag.fraction", default=0.5, lower=0, upper=1),
      makeNumericLearnerParam(id="train.fraction", default=1, lower=0, upper=1)
    ),
    twoclass = TRUE,
    missings = TRUE,
    numerics = TRUE,
    factors = TRUE,
    prob = TRUE,
    weights = TRUE
  )
}

#' @S3method trainLearner classif.gbm
trainLearner.classif.gbm = function(.learner, .task, .subset, .weights = NULL,  ...) {
  d = getTaskData(.task, .subset, recode.target="01")
  if (is.null(.weights)) {
    f = getTaskFormula(.task)
    gbm(f, data=d, keep.data=FALSE, verbose=FALSE, ...)
  } else  {
    f = as.formula(getTaskFormulaAsString(.task))
    gbm(f, data=d, keep.data=FALSE, verbose=FALSE, weights=.weights, ...)
  }
}

#' @S3method predictLearner classif.gbm
predictLearner.classif.gbm = function(.learner, .model, .newdata, ...) {
  m = .model$learner.model
  p = predict(m, newdata=.newdata, type="response", n.trees=length(m$trees), single.tree=FALSE, ...)
  levs = c(.model$task.desc$negative, .model$task.desc$positive)
  if (.learner$predict.type == "prob") {
    y = matrix(0, ncol=2, nrow=nrow(.newdata))
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
