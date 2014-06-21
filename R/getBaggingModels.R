#' Returns the list of models fitted in bagging.
#'
#' @param model [\code{\link{WrappedModel}}]\cr
#'   Model produced by training a bagging learner.
#' @param learner.models [\code{logical(1)}]\cr
#'   Return underlying R models (e.g., rpart models) or
#'   wrapped mlr models (\code{\link{WrappedModel}}).
#'   Default is \code{FALSE}.
#' @return [\code{list}].
#' @export
getBaggingModels = function(model, learner.models=FALSE) {
  checkArg(model, c("BaggingModel","OverBaggingModel"))
  ms = model$learner.model$next.model
  if (learner.models)
    extractSubList(ms, "learner.model", simplify=FALSE)
  else
    ms
}
