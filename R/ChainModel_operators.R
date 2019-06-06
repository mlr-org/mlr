getLeafModel = function(model) {
  if (inherits(model, "BaseWrapperModel")) {
    return(getLeafModel(model$learner.model$next.model))
  }
  return(model)
}
