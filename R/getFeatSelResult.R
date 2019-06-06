#' Returns the selected feature set and optimization path after training.
#'
#' @param object ([WrappedModel])\cr
#'   Trained Model created with [makeFeatSelWrapper].
#' @return ([FeatSelResult]).
#' @export
#' @family featsel
getFeatSelResult = function(object) {
  assertClass(object, "FeatSelModel")
  object$learner.model$opt.result
}
