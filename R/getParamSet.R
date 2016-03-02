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
