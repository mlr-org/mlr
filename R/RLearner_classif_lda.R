#' @S3method makeRLearner classif.lda
makeRLearner.classif.lda = function() {
  makeRLearnerClassif(
    cl = "classif.lda",
    package = "MASS",
    par.set = makeParamSet(
      makeDiscreteLearnerParam(id="method", default="moment", values=c("moment", "mle", "mve", "t")),
      makeNumericLearnerParam(id="nu", lower=2, requires=expression(method=="t")),
      makeNumericLearnerParam(id="tol", default=1e-4, lower=0)
    ),
    twoclass=TRUE,
    multiclass = TRUE,
    numerics = TRUE,
    factors = TRUE,
    prob = TRUE
  )
}

#' @S3method trainLearner classif.lda
trainLearner.classif.lda = function(.learner, .task, .subset, .weights = NULL,  ...) {
  f = getTaskFormula(.task)
  lda(f, data=getTaskData(.task, .subset), ...)
}

#' @S3method predictLearner classif.lda
predictLearner.classif.lda = function(.learner, .model, .newdata, ...) {
  p = predict(.model$learner.model, newdata=.newdata, ...)
  if(.learner$predict.type == "response")
    return(p$class)
  else
    return(p$posterior)
}

