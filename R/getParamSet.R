#' @title Get a description of all possible parameter settings for a learner.
#'
#' @template arg_learner
#' @template ret_ps
#' @family learner
#' @name getParamSet
#' @rdname getParamSet
NULL

#' @export
getParamSet.Learner = function(x) {
  x$par.set
}

#' @export
getParamSet.character = function(x) {
  x = checkLearner(x)
  getParamSet(x)
}
