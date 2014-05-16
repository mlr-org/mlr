#' @S3method makeRLearner classif.geoDA
makeRLearner.classif.geoDA = function() {
  makeRLearnerClassif(
    cl = "classif.geoDA",
    package = "DiscriMiner",
    par.set = makeParamSet(),
    twoclass = TRUE,
    multiclass = TRUE,
    numerics = TRUE,
    factors = FALSE,
    missings = FALSE
  )
}

#' @S3method trainLearner classif.geoDA
trainLearner.classif.geoDA = function(.learner, .task, .subset, .weights = NULL,  ...) {
  d = getTaskData(.task, .subset, target.extra=TRUE)
  geoDA(variables = d$data, group = d$target)
}

#' @S3method predictLearner classif.geoDA
predictLearner.classif.geoDA = function(.learner, .model, .newdata, ...) {
  m = .model$learner.model
  p = classify(m, newdata = .newdata)
  #p$scores #we loose this information
  p$pred_class
}
