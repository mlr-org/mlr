#'@export
getParamSet.Learner = function(x, ...) {
  x$par.set
}

#'@export
getParamSet.character = function(x, ...) {
  x = checkLearner(x)
  getParamSet(x, ...)
}
