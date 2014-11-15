makeChainModel = function(next.model, cl) {
  setClasses(list(next.model = next.model), c(cl, "ChainModel", "WrappedModel"))
}


#' @export
makeWrappedModel.BaseWrapper = function(learner, learner.model, task.desc, subset, features, factor.levels, time) {
  x = NextMethod()
  addClasses(x, "BaseWrapperModel")
}

#'@export
getLearnerModel.BaseWrapperModel = function(model) {
  model$learner.model$next.model$learner.model
}

#' @export
print.BaseWrapperModel = function(x, ...) {
  leafm = getLeafModel(x)
  catf("Learner model for id=%s chain=%s",
    x$learner$id, class(x$learner)[1L])
  catf("[chain] Trained on obs: %i", length(x$subset))
  catf("[chain] Used features: %i", length(x$features))
  catf("[model] Trained on obs: %i", length(leafm$subset))
  catf("[model] Used features: %i", length(leafm$features))
  catf("Hyperparameters: %s", getHyperParsString(x$learner))
  if (isFailureModel(x))
    catf("Training failed: %s", getFailureModelMsg(x))
}

