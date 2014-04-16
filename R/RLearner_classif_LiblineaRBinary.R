#' @S3method makeRLearner classif.LiblineaRBinary
makeRLearner.classif.LiblineaRBinary = function() {
  makeRLearnerClassif(
    cl = "classif.LiblineaRBinary",
    package = "LiblineaR",
    par.set = makeParamSet(
      makeDiscreteLearnerParam(id = "type", values = c(1,2,3,5)),
      makeNumericLearnerParam(id = "cost", default = 1, lower = 0),
      # better default epsilon is dependent on type (=NULL, see docs), but we cannot store this
      makeNumericLearnerParam(id = "epsilon", lower = 0),
      makeLogicalLearnerParam(id = "bias", default = TRUE),
      makeNumericVectorLearnerParam(id = "wi", len = NA_integer_),
      makeIntegerLearnerParam(id = "cross", default = 0L, lower = 0L),
      makeLogicalLearnerParam(id = "verbose", default = FALSE)
    ),
    twoclass = TRUE,
    multiclass = FALSE,
    missings = FALSE,
    numerics = TRUE,
    factors = FALSE,
    prob = FALSE,
    weights = FALSE
  )
}

#' @S3method trainLearner classif.LiblineaRBinary
trainLearner.classif.LiblineaRBinary = function(.learner, .task, .subset, .weights, ...) {
  d = getTaskData(.task, .subset, target.extra = TRUE)
  LiblineaR(data = d$data, labels = d$target, ...)
}

#' @S3method predictLearner classif.LiblineaRBinary
predictLearner.classif.LiblineaRBinary = function(.learner, .model, .newdata, ...) {
  p = as.factor(predict(.model$learner.model, newx = .newdata, ...)$predictions)
}
