checkLearner = function(learner, type = NULL, weights = FALSE, ...) {
  if (is.character(learner))
    learner = makeLearner(learner)
  else
    checkArg(learner, "Learner")
  if (!is.null(type) && learner$type != type)
    stopf("Learner '%s' must be of type %s, not: %s", learner$id, type, learner$type)
  if (weights && !hasProperties(learner, "weights"))
    stopf("Learner '%s' must support weights, but does not!", learner$id)
  setHyperPars(learner, ...)
}

checkLearnerClassif = function(learner, weights = FALSE) {
  checkLearner(learner, "classif", weights)
}

checkLearnerRegr = function(learner, weights = FALSE) {
  checkLearner(learner, "regr", weights)
}
