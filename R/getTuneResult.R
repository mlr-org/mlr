#' Returns the optimal hyperparameters and optimization path after training.
#'
#' @param object [\code{\link{WrappedModel}}]\cr
#'   Trained Model created with \code{\link{makeTuneWrapper}}.
#' @return [\code{\link{TuneResult}}].
#' @family tune
#' @export
getTuneResult = function(object) {
  assertClass(object, "TuneModel")
  object$learner.model$opt.result
}

