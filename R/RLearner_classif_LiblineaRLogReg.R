#' @S3method makeRLearner classif.LiblineaRLogReg
makeRLearner.classif.LiblineaRLogReg = function() {
  makeRLearnerClassif(
    cl = "classif.LiblineaRLogReg",
    package = "LiblineaR",
    par.set = makeParamSet(
      # FIXME: type is more a categorical param....
      makeDiscreteLearnerParam(id="type", values=c(0,6,7)),
      makeNumericLearnerParam(id="cost", default=1, lower=0),
      #FIXME: Default epsilon is dependet on type
      #0,6: 0.01, 7: 0.1
      makeNumericLearnerParam(id="epsilon", default=0.01, lower=0),
      makeLogicalLearnerParam(id="bias", default=TRUE),
      makeNumericVectorLearnerParam(id="wi", len=NA_integer_),
      makeIntegerLearnerParam(id="cross", default=0L, lower=0L),
      makeLogicalLearnerParam(id="verbose", default=FALSE)
    ),
    # par.vals = list(type=)
    twoclass = TRUE,
    multiclass = FALSE,
    missings = FALSE,
    numerics = TRUE,
    factors = FALSE,
    prob = TRUE,
    weights = FALSE
  )
}

#' @S3method trainLearner classif.LiblineaRLogReg
trainLearner.classif.LiblineaRLogReg = function(.learner, .task, .subset, .weights,  ...) {
  d = getTaskData(.task, .subset, target.extra=TRUE)
  LiblineaR(data=d$data, labels=d$target, ...)
}

#' @S3method predictLearner classif.LiblineaRLogReg
predictLearner.classif.LiblineaRLogReg = function(.learner, .model, .newdata, ...) {
  if(.learner$predict.type == "response")
    p = as.factor(predict(.model$learner.model, newx=.newdata, ...)$predictions)
  else
    p = predict(.model$learner.model, newx=.newdata, proba=TRUE, ...)$probabilities
}
