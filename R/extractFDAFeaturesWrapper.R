#' @title Fuse learner with an extractFDAFeatures method.
#'
#' @description
#' Fuses a base learner with an extractFDAFeatures method. Creates a learner object, which can be
#' used like any other learner object.
#' Internally uses \code{\link{extractFDAFeatures}} before training the learner and
#' \code{\link{reExtractFDAFeatures}} before predicting.
#'
#' @template arg_learner
#' @inheritParams extractFDAFeatures
#' @export
#' @family extractFDAFeatures
#' @family wrapper
#' @template ret_learner
makeExtractFDAFeatsWrapper = function(learner, feat.methods = list(),
  fd.features = list(), fd.grids = list()) {

  learner = checkLearner(learner)
  args = list(feat.methods = feat.methods, fd.features = fd.features, fd.grids = fd.grids)
  rm(list = names(args))

  trainfun = function(data, target, args) {
    setNames(do.call(extractFDAFeatures, c(list(obj = data, target = target), args)),
      c("data", "control"))
  }

  predictfun = function(data, target, args, control) {
    reExtractFDAFeatures(data, control)
  }

  lrn = makePreprocWrapper(learner, trainfun, predictfun, par.vals = args)
  lrn$id = stri_replace(lrn$id, replacement = ".extracted", regex = "[.]preproc$")
  addClasses(lrn, "extractFDAFeatsWrapper")
}
