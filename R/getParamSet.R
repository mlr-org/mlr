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
getParamSet.Learner = function(x, ...) {
  x$par.set
}

#'@export
getParamSet.character = function(x, ...) {
  x = checkLearner(x)
  getParamSet(x, ...)
}


