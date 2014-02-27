#' @S3method makeRLearner classif.LiblineaRMultiClass
makeRLearner.classif.LiblineaRMultiClass = function() {
  makeRLearnerClassif(
    cl = "classif.LiblineaRMultiClass",
    package = "LiblineaR",
    par.set = makeParamSet(
      # FIXME: type is more a categorical param....
      makeDiscreteLearnerParam(id="type", values=c(4)),
      makeNumericLearnerParam(id="cost", default=1, lower=0),
      makeNumericLearnerParam(id="epsilon", default=0.01, lower=0),
      makeLogicalLearnerParam(id="bias", default=TRUE),
      makeNumericVectorLearnerParam(id="wi", len=NA_integer_),
      makeIntegerLearnerParam(id="cross", default=0L, lower=0L),
      makeLogicalLearnerParam(id="verbose", default=FALSE)
    ),
    par.vals = list(type=4, epsilon=0.1)
    twoclass = TRUE,
    multiclass = TRUE,
    missings = FALSE,
    numerics = TRUE,
    factors = FALSE,
    prob = FALSE,
    weights = FALSE
  )
}

#' @S3method trainLearner classif.LiblineaRMultiClass
trainLearner.classif.LiblineaRMultiClass = function(.learner, .task, .subset, .weights, ...) {
  d = getTaskData(.task, .subset, target.extra=TRUE)
  LiblineaR(data=d$data, labels=d$target, ...)
}

#' @S3method predictLearner classif.LiblineaRMultiClass
predictLearner.classif.LiblineaRMultiClass = function(.learner, .model, .newdata, ...) {
  p = as.factor(predict(.model$learner.model, newx=.newdata, ...)$predictions)
}
