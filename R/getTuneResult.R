#' Returns the optimal hyperparameters and optimization path after training.
#'
#' @param object ([WrappedModel])\cr
#'   Trained Model created with [makeTuneWrapper].
#' @return ([TuneResult]).
#' @family tune
#' @export
getTuneResult = function(object) {
  assertClass(object, "TuneModel")
  object$learner.model$opt.result
}
