makeChainModel = function(next.model, cl) {
  setClasses(list(next.model = next.model), c(cl, "ChainModel", "WrappedModel"))
}


#'@export
getLearnerModel.BaseWrapperModel = function(model) {
  # FIXME: this structure and special-cases really suck. FailureModel and NoFeaturesModel
  # should probably be redesigned at some point
  if (inherits(model$learner.model, "NoFeaturesModel"))
    return(model$learner.model)
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

