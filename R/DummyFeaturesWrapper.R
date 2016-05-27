makeDummyFeaturesWrapper = function (learner) 
{
  #learner = checkLearner(learner)
  trainfun = function(data, target, args) {
    data = createDummyFeatures(data, target)
    return(list(data = data, control = list()))
  }
  predictfun = function(data, target, args, control) {
    createDummyFeatures(data, target)
  }
  lrn = makePreprocWrapper(learner, trainfun, predictfun)
  addClasses(lrn, "DummyFeaturesWrapper")
}

getLearnerProperties.DummyFeaturesWrapper = function (learner) {
  union(getLearnerProperties(learner$next.learner), c("factors", "ordered"))
}
