#' @title Return error message of FailureModel.
#'
#' @description
#' Such a model is created when one sets the corresponding option in \code{\link{configureMlr}}.
#' If no failure occurred, \code{NA} is returned.
#'
#' For complex wrappers this getter returns the first error essage encountered in ANY model that failed.
#'
#' @template arg_wrappedmod
#' @return [\code{character(1)}].
#' @export
getFailureModelMsg = function(model) {
  UseMethod("getFailureModelMsg")
}

#' @export
getFailureModelMsg.WrappedModel = function(model) {
  if (inherits(model, "FailureModel"))
    return(as.character(model$learner.model))
  if (inherits(model, "BaseWrapperModel"))
    return(getFailureModelMsg(model$learner.model$next.model))
  return(NA_character_)
}

#' @export
getFailureModelMsg.BaggingModel = function(model) {
  mods = getBaggingModels(model, learner.models = FALSE)
  msgs = sapply(mods, getFailureModelMsg)
  j = which.first(!is.na(msgs))
  ifelse(j == 0L, NA_character_ , msgs[j])
}

#' @export
getFailureModelMsg.CostSensWeightedPairsModel = function(model) {
  mods = getCostSensWeightedPairsModels(model, learner.models = FALSE)
  msgs = sapply(mods, getFailureModelMsg)
  j = which.first(!is.na(msgs))
  ifelse(j == 0L, NA_character_ , msgs[j])
}

