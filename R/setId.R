#' @title Set the id of a learner object.
#'
#' @description
#' Deprecated, use [setLearnerId] instead.
#'
#' @template arg_learner
#' @param id (`character(1)`)\cr
#'    New id for learner.
#' @template ret_learner
#' @export
#' @family learner
setId = function(learner, id) {
  .Deprecated("setLearnerId")
  learner = checkLearner(learner)
  assertString(id)
  learner$id = id
  return(learner)
}
