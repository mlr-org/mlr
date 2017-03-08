#' @export
makeRLearner.classif.featureless = function() {
  makeRLearnerClassif(
    cl = "classif.featureless",
    package = "mlr",
    par.set = makeParamSet(addClasses(makeUntypedLearnerParam(id = "measure", default = getDefaultMeasure("classif")), "MeasureParam")),
    par.vals = list(measure = mmce),
    properties = c("twoclass", "multiclass", "numerics", "factors", "ordered", "missings", "prob"),
    name = "Featureless classifier",
    short.name = "featureless"
  )
}


#' @export
trainLearner.classif.featureless = function(.learner, .task, .subset, .weights = NULL, measure,...) {
  constantPrediction(.task, measure, predict.type = .learner$predict.type)
}


#' @export
predictLearner.classif.featureless = function(.learner, .model, .newdata, ...) {
  res = replicate(nrow(.newdata), .model$learner.model)
  
  if (is.matrix(res)) 
    res = t(res)
  
  res
}
