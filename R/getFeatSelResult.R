#' Returns the selected feature set and optimization path after training.
#'
#' @param object [\code{\link{WrappedModel}}]\cr
#'   Trained Model created with \code{\link{makeFeatSelWrapper}}.
#' @return [\code{\link{FeatSelResult}}].
#' @export
#' @family featsel
getFeatSelResult = function(object) {
  assertClass(object, "FeatSelModel")
  object$learner.model$opt.result
}


