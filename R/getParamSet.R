#' Get a description of all possible parameter settings for a learner.
#'
#' @template arg_learner
#' @template ret_ps
#' @family learner
#' @export
getParamSet = function(learner) {
  assertClass(learner, classes = "Learner")
  UseMethod("getParamSet")
}

#'@export
getParamSet.Learner = function(learner) {
  assertClass(learner, classes = "Learner")
  learner$par.set
}

