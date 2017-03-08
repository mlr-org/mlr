#' @export
makeRLearner.regr.featureless = function() {
  makeRLearnerRegr(
    cl = "regr.featureless",
    package = "mlr",
    par.set = makeParamSet(addClasses(makeUntypedLearnerParam(id = "measure", default = mse), "MeasureParam")),
    par.vals = list(measure = mse),
    properties = c("numerics", "factors", "ordered", "missings"),
    name = "Featureless regression",
    short.name = "featureless"
  )
}

#' @export
trainLearner.regr.featureless = function(.learner, .task, .subset, .weights = NULL, measure,...) {
  constantPrediction(.task, measure)
}


#' @export
predictLearner.regr.featureless = function(.learner, .model, .newdata, ...) {
  rep(.model$learner.model, times = nrow(.newdata))
}
