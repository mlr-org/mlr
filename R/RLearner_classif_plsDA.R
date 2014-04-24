#' @S3method makeRLearner classif.plsDA
makeRLearner.classif.plsDA = function() {
  makeRLearnerClassif(
    cl = "classif.plsDA",
    package = "DiscriMiner",
    par.set = makeParamSet(
      makeLogicalLearnerParam(id="autosel", default=TRUE),
      makeIntegerLearnerParam(id="comps", lower=1L, default=2L, requires=expression(autosel==TRUE)),
      makeLogicalLearnerParam(id="retain.models", default=FALSE)
      ),
    twoclass = TRUE,
    multiclass = TRUE,
    numerics = TRUE,
    factors = FALSE,
    missings = FALSE
  )
}

#' @S3method trainLearner classif.plsDA
trainLearner.classif.plsDA = function(.learner, .task, .subset, .weights,  ...) {
  d = getTaskData(.task, .subset, target.extra=TRUE)
  plsDA(variables = d$data, group = d$target, ...)
}

#' @S3method predictLearner classif.plsDA
predictLearner.classif.plsDA = function(.learner, .model, .newdata, ...) {
  m = .model$learner.model
  p = classify(m, newdata = .newdata)
  #p$scores #we loose this information
  p$pred_class
}
