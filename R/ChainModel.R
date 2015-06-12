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


