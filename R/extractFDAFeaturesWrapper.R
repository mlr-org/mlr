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

  # FIXME:
  # This is stupid, we can not handle multiple tasks for a single wrapper this way.
  # (Impute cant do this neither if using cols = list("X1" = ...)).
  # One solution would be to be able to specify "all" features (regexp would be overkill?).

  learner = checkLearner(learner)
  args = list(feat.methods = feat.methods)
  rm(list = names(args))

  trainfun = function(data, target, args) {
    l = do.call(extractFDAFeatures, c(list(obj = data, target = target), args))
    names(l) =  c("data", "control")
    l
  }

  predictfun = function(data, target, args, control) {
    reextractFDAFeatures(data, control)
  }

  lrn = makePreprocWrapper(learner, trainfun, predictfun, par.vals = args)
  lrn$id = stri_replace(lrn$id, replacement = ".extracted", regex = "[.]preproc$")
  addClasses(lrn, "extractFDAFeatsWrapper")
}

#' @export
getLearnerProperties.extractFDAFeatsWrapper = function(learner) {
  union(getLearnerProperties(learner$next.learner), c("functionals", "single.functional"))
}
