#' @title Fuse learner with dummy feature creator.
#'
#' @description
#' Fuses a base learner with the dummy feature creator. Creates a learner object, which can be
#' used like any other learner object.
#' 
#' @template arg_learner
#' @param method [\code{character(1)}]\cr
#'   Available are:\cr
#'   \dQuote{1-of-n}: For n factor levels there will be n dummy variables.\cr
#'   \dQuote{reference}: There will be n-1 dummy variables leaving out the first factor level of each variable.\cr
#' @param cols [\code{character}]\cr
#'   Columns to create dummy features for. Default is to use all columns.
#' @template ret_taskdf
#' @export
#' @family wrapper
makeDummyFeaturesWrapper = function (learner, method = "1-of-n", cols = NULL) 
{
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
  lrn$id = stri_replace(lrn$id, replacement = ".dummied", regex = "[.]preproc$")
  addClasses(lrn, "DummyFeaturesWrapper")
}

getLearnerProperties.DummyFeaturesWrapper = function (learner) {
  union(getLearnerProperties(learner$next.learner), c("factors", "ordered"))
}
