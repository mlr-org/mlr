#' @title Fuse learner with an extractFDAFeatures method.
#'
#' @description
#' Fuses a base learner with an extractFDAFeatures method. Creates a learner object, which can be
#' used like any other learner object.
#' Internally uses [extractFDAFeatures] before training the learner and
#' [reextractFDAFeatures] before predicting.
#'
#' @template arg_learner
#' @inheritParams extractFDAFeatures
#' @export
#' @family fda
#' @family wrapper
#' @template ret_learner
makeExtractFDAFeatsWrapper = function(learner, feat.methods = list()) {

  assertList(feat.methods, names = "named")

  learner = checkLearner(learner)
  args = list(feat.methods = feat.methods)
  rm(list = names(args))

  trainfun = function(data, target, args) {
    lst = do.call(extractFDAFeatures, c(list(obj = data, target = target), args))
    names(lst) = c("data", "control")
    return(lst)
  }

  predictfun = function(data, target, args, control) {
    reextractFDAFeatures(data, control)
  }
  ps = do.call("c", extractSubList(args$feat.methods, "par.set", simplify = FALSE))

  lrn = makePreprocWrapper(learner, trainfun, predictfun, par.vals = args, par.set = ps)
  lrn$id = stri_replace(lrn$id, replacement = ".extracted", regex = "[.]preproc$")
  addClasses(lrn, "extractFDAFeatsWrapper")
}

#' @export
getLearnerProperties.extractFDAFeatsWrapper = function(learner) {
  union(getLearnerProperties(learner$next.learner), c("functionals", "single.functional"))
}
