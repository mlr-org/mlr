#' Get a description of all possible parameter settings for a learner.
#'
#' @template arg_learner
#' @template ret_ps
#' @family learner
#' @export
getParamSet = function(x, ...) {
  checkLearner(learner)
  UseMethod("getParamSet")
}

#'@export
getParamSet.Learner = function(learner) {
  learner$par.set
}

#'@export
getParamSet.character = function(learner) {
  learner = checkLearner(learner)
  getParamSet(learner)
}


