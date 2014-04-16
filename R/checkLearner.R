checkLearner = function(learner, type) {
  checkArg(learner, "Learner")
  if (learner$type != type)
    stopf("Learner '%s' must be of type %s, not: %s", learner$id, type, learner$type)
}

checkLearnerClassif = function(learner) {
  checkLearner(learner, "classif")
}

checkLearnerRegr = function(learner) {
  checkLearner(learner, "regr")
}
