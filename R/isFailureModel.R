
#' @title Is the model a FailureModel?
#'
#' @description
#' Such a model is created when one sets the corresponding option in \code{\link{configureMlr}}.
#'
#' For complex wrappers this getter returns \code{TRUE} if ANY model contained in it failed.
#'
#' @template arg_wrappedmod
#' @return [\code{logical(1)}].
#' @export
isFailureModel = function(model) {
  UseMethod("isFailureModel")
}

#' @export
isFailureModel.WrappedModel = function(model) {
  if (inherits(model, "FailureModel"))
    return(TRUE)
  if (inherits(model, "BaseWrapperModel"))
    return(isFailureModel(model$learner.model$next.model))
  return(FALSE)
}

#' @export
isFailureModel.BaggingModel = function(model) {
  mods = getBaggingModels(model, learner.models = FALSE)
  isit = vlapply(mods, isFailureModel)
  return(any(isit))
}

#' @export
isFailureModel.MulticlassModel = function(model) {
  mods = model$learner.model$next.model$models
  isit = vlapply(mods, isFailureModel)
  return(any(isit))
}

#' @export
isFailureModel.CostSensWeightedPairsModel = function(model) {
  mods = getCostSensWeightedPairsModels(model, learner.models = FALSE)
  isit = vlapply(mods, isFailureModel)
  return(any(isit))
}
