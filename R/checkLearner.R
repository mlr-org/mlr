# props checks if Learner has one or more properties specified in props as a 
# character vector.

checkLearner = function(learner, type = NULL, props = NULL, ...) {
  if (is.character(learner))
    learner = makeLearner(learner)
  else
    assertClass(learner, classes = "Learner")
  if (!is.null(props)){
    assertSubset(props, getSupportedLearnerProperties())
    learner.props = getLearnerProperties(learner)
    missing.props = setdiff(props, learner.props)
    if (length(missing.props) != 0){
      stopf("Learner '%s' must support all properties '%s', but does not support '%s'.", learner$id, collapse(props), collapse(missing.props))
    }
  }
  if (!is.null(type) && learner$type %nin% type)
    stopf("Learner '%s' must be of type '%s', not: '%s'", learner$id, collapse(type), learner$type)
  setHyperPars(learner, ...)
}

checkLearnerClassif = function(learner, props = NULL) {
  checkLearner(learner, "classif", props)
}

checkLearnerRegr = function(learner, props = NULL) {
  checkLearner(learner, "regr", props)
}

