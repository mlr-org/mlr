#' @title Set the probability threshold the learner should use.
#'
#' @description
#' See `predict.threshold` in [makeLearner] and [setThreshold].
#'
#' For complex wrappers only the top-level `predict.type` is currently set.
#'
#' @template arg_learner
#' @template arg_predictthreshold
#' @template ret_learner
#' @family predict
#' @family learner
#' @export
setPredictThreshold = function(learner, predict.threshold) {
  learner = checkLearner(learner, type = "classif")
  if (learner$predict.type != "prob") {
    stopf("predict.type = 'prob' must hold to set a predict.threshold!")
  }
  assertNumeric(predict.threshold, any.missing = FALSE)
  learner$predict.threshold = predict.threshold
  return(learner)
}
