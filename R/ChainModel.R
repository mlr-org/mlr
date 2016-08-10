makeChainModel = function(next.model, cl) {
  setClasses(list(next.model = next.model), c(cl, "ChainModel", "WrappedModel"))
}


#'@export
getLearnerModel.BaseWrapperModel = function(model, more.unwrap = FALSE) {
  # FIXME: this structure and special-cases really suck. FailureModel and NoFeaturesModel
  # should probably be redesigned at some point
  if (inherits(model$learner.model, "NoFeaturesModel"))
    return(model$learner.model)
  if (more.unwrap)
    model$learner.model$next.model$learner.model
  else
    model$learner.model$next.model
}

#' @export
print.ChainModel = function(x, ...) {
  print(x$next.model)
}


