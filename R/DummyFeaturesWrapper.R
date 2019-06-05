#' @title Fuse learner with dummy feature creator.
#'
#' @description
#' Fuses a base learner with the dummy feature creator (see [createDummyFeatures]).
#' Returns a learner which can be used like any other learner.
#'
#' @template arg_learner
#' @inheritParams createDummyFeatures
#' @template ret_learner
#' @family wrapper
#' @export
makeDummyFeaturesWrapper = function(learner, method = "1-of-n", cols = NULL) {

  learner = checkLearner(learner)
  args = list(method = method, cols = cols)
  rm(list = names(args))

  trainfun = function(data, target, args) {
    data = do.call(createDummyFeatures, c(list(obj = data, target = target), args))
    return(list(data = data, control = list()))
  }

  predictfun = function(data, target, args, control) {
    y = intersect(target, colnames(data))
    data = do.call(createDummyFeatures, c(list(obj = data, target = y), args))
    return(data)
  }

  lrn = makePreprocWrapper(learner, trainfun, predictfun, par.vals = args)
  lrn$id = stri_replace(lrn$id, replacement = ".dummied", regex = "\\.preproc$")
  addClasses(lrn, "DummyFeaturesWrapper")
}

getLearnerProperties.DummyFeaturesWrapper = function(learner) {
  union(getLearnerProperties(learner$next.learner), c("factors", "ordered"))
}
