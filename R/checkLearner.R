checkLearner = function(learner, type = NULL, weights = FALSE, ...) {
  if (is.character(learner))
    learner = makeLearner(learner)
  else
    assertClass(learner, classes = "Learner")
  if (!is.null(type) && learner$type %nin% type)
    stopf("Learner '%s' must be of type '%s', not: '%s'", learner$id, collapse(type, ','), learner$type)
  if (weights && !hasLearnerProperties(learner, "weights"))
    stopf("Learner '%s' must support weights, but does not!", learner$id)
  setHyperPars(learner, ...)
}

checkLearnerClassif = function(learner, weights = FALSE) {
  checkLearner(learner, "classif", weights)
}

checkLearnerRegr = function(learner, weights = FALSE) {
  checkLearner(learner, "regr", weights)
}
