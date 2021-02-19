#' @title Fuse learner with removal of constant features preprocessing.
#'
#' @description
#' Fuses a base learner with the preprocessing implemented in [removeConstantFeatures].
#'
#' @template arg_learner
#' @inheritParams removeConstantFeatures
#' @export
#' @family wrapper
#' @template ret_learner
makeRemoveConstantFeaturesWrapper = function(learner, perc = 0, dont.rm = character(0L), na.ignore = FALSE, wrap.tol = .Machine$double.eps^.5) {

  learner = checkLearner(learner)
  args = list(perc = perc, dont.rm = dont.rm, na.ignore = na.ignore, wrap.tol = wrap.tol)
  rm(list = names(args))

  trainfun = function(data, target, args) {
    args$dont.rm = union(args$dont.rm, target)
    tmp = do.call(removeConstantFeatures, c(list(obj = data), args))
    list(data = tmp, control = list(dropped.cols = setdiff(names(data), names(tmp))))
  }

  predictfun = function(data, target, args, control) {
    dropNamed(data, control$dropped.cols)
  }

  lrn = makePreprocWrapper(learner, trainfun, predictfun,
    par.set = makeParamSet(
      makeNumericLearnerParam(id = "perc", lower = 0),
      makeUntypedLearnerParam(id = "dont.rm", default = character(0L)),
      makeLogicalLearnerParam(id = "na.ignore", default = FALSE),
      makeNumericLearnerParam(id = "wrap.tol")
    ),
    par.vals = args)
  addClasses(lrn, "RemoveConstantFeaturesWrapper")
}
