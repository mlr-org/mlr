#' Get a description of all possible parameter settings for a learner.
#'
#' @template arg_learner
#' @template ret_ps
#' @family learner
#' @export
getParamSet = function(learner) {
  checkLearner(learner)
  UseMethod("getParamSet")
}

#'@export
getParamSet.Learner = function(learner) {
  assertClass(learner, classes = "Learner")
  learner$par.set
}

#'@export
getParamSet.character = function(learner) {
  learner = checkLearner(learner)
  getParamSet(learner)
}


