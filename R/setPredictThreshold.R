#' @title Set the probability threshold the learner should use.
#'
#' @description
#' See \code{predict.threshold} in \code{\link{makeLearner}} and \code{\link{setThreshold}}.
#'
#' For complex wrappers only the top-level \code{predict.type} is currently set.
#'
#' @template arg_learner
#' @template arg_predictthreshold
#' @template ret_learner
#' @family predict
#' @family learner
#' @export
setPredictThreshold = function(learner, predict.threshold) {
  learner = checkLearner(learner, type = "classif")
  assertNumeric(predict.threshold, any.missing = FALSE)
  learner$predict.threshold = predict.threshold
  return(learner)
}


