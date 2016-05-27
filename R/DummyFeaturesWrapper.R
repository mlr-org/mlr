makeDummyFeaturesWrapper = function (learner) 
{
  learner = checkLearner(learner)
  trainfun = function(data, target, args) {
    data = createDummyFeatures(data, target)
    return(list(data = data, control = list()))
  }
  predictfun = function(data, target, args, control) {
    if (any(target %in% colnames(data))) 
      data = createDummyFeatures(data, target) else
        data = createDummyFeatures(data)
    return(data)
  }
  lrn = makePreprocWrapper(learner, trainfun, predictfun)
  lrn$id = gsub("[.]preproc", ".dummied", lrn$id)
  addClasses(lrn, "DummyFeaturesWrapper")
}

getLearnerProperties.DummyFeaturesWrapper = function (learner) {
  union(getLearnerProperties(learner$next.learner), c("factors", "ordered"))
}
