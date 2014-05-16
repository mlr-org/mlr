#' @S3method makeRLearner classif.naiveBayes
makeRLearner.classif.naiveBayes = function() {
  makeRLearnerClassif(
    cl = "classif.naiveBayes",
    package = "e1071",
    par.set = makeParamSet(
      makeNumericLearnerParam(id="laplace", default=0, lower=0)
      # makeNumericLearnerParam(id="threshold", default=0.001, lower=0)
    ),
    twoclass = TRUE,
    multiclass = TRUE,
    missings = TRUE,
    numerics = TRUE,
    factors = TRUE,
    prob = TRUE
  )
}

#' @S3method trainLearner classif.naiveBayes
trainLearner.classif.naiveBayes = function(.learner, .task, .subset, .weights = NULL,  ...) {
  f = getTaskFormula(.task)
  naiveBayes(f, data=getTaskData(.task, .subset), ...)
}

#' @S3method predictLearner classif.naiveBayes
predictLearner.classif.naiveBayes = function(.learner, .model, .newdata, ...) {
  type = ifelse(.learner$predict.type=="response", "class", "raw")
  predict(.model$learner.model, newdata=.newdata, type=type, ...)
}
