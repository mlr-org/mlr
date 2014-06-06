#' @export
makeRLearner.classif.geoDA = function() {
  makeRLearnerClassif(
    cl = "classif.geoDA",
    package = "DiscriMiner",
    par.set = makeParamSet(),
    properties = c("twoclass", "multiclass", "numerics")
  )
}

#' @export
trainLearner.classif.geoDA = function(.learner, .task, .subset, .weights = NULL,  ...) {
  d = getTaskData(.task, .subset, target.extra=TRUE)
  geoDA(variables = d$data, group = d$target, ...)
}

#' @export
predictLearner.classif.geoDA = function(.learner, .model, .newdata, ...) {
  m = .model$learner.model
  p = classify(m, newdata = .newdata)
  #p$scores #we loose this information
  p$pred_class
}
