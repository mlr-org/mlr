checkLearner = function(learner, type, weights = FALSE) {
  checkArg(learner, "Learner")
  if (learner$type != type)
    stopf("Learner '%s' must be of type %s, not: %s", learner$id, type, learner$type)
  if (weights && !learner$weights)
    stopf("Learner '%s' must support weights, but does not!", learner$id)
}

checkLearnerClassif = function(learner, weights = FALSE) {
  checkLearner(learner, "classif", weights)
}

checkLearnerRegr = function(learner, weights = FALSE) {
  checkLearner(learner, "regr", weights)
}
