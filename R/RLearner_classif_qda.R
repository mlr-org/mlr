#' @S3method makeRLearner classif.qda
makeRLearner.classif.qda = function() {
  makeRLearnerClassif(
    cl = "classif.qda",
    package = "MASS",
    par.set = makeParamSet(
      makeDiscreteLearnerParam(id="method", default="moment", values=c("moment", "mle", "mve", "t")),
      makeNumericLearnerParam(id="nu", default=5 , lower=2, requires=expression(method == "t"))
    ),
    twoclass = TRUE,
    multiclass = TRUE,
    numerics = TRUE,
    factors = TRUE,
    prob = TRUE
  )
}

#' @S3method trainLearner classif.qda
trainLearner.classif.qda = function(.learner, .task, .subset, .weights = NULL,  ...) {
  f = getTaskFormula(.task)
  qda(f, data=getTaskData(.task, .subset), ...)
}

#' @S3method predictLearner classif.qda
predictLearner.classif.qda = function(.learner, .model, .newdata, ...) {
  p = predict(.model$learner.model, newdata=.newdata, ...)
  if(.learner$predict.type == "response")
    return(p$class)
  else
    return(p$posterior)
}
