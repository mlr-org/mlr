#' Get a description of all possible parameter settings for a learner.
#'
#' @param learner [\code{\link{Learner}}]\cr
#'   The learner.
#' @return [\code{\link[ParamHelpers]{ParamSet}}].
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

