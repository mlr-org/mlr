#' @title Fuse learner with an imputation method.
#'
#' @description
#' Fuses a base learner with an imputation method. Creates a learner object, which can be
#' used like any other learner object.
#' Internally uses [impute] before training the learner and [reimpute]
#' before predicting.
#'
#' @template arg_learner
#' @inheritParams impute
#' @export
#' @family impute
#' @family wrapper
#' @template ret_learner
makeImputeWrapper = function(learner, classes = list(), cols = list(),
  dummy.classes = character(0L), dummy.cols = character(0L), dummy.type = "factor", force.dummies = FALSE, impute.new.levels = TRUE, recode.factor.levels = TRUE) {

  learner = checkLearner(learner)
  args = list(classes = classes, cols = cols,
    dummy.classes = dummy.classes, dummy.cols = dummy.cols, dummy.type = dummy.type, force.dummies = force.dummies,
    impute.new.levels = impute.new.levels, recode.factor.levels = recode.factor.levels)
  rm(list = names(args))

  trainfun = function(data, target, args) {
    setNames(do.call(impute, c(list(obj = data, target = target), args)), c("data", "control"))
  }

  predictfun = function(data, target, args, control) {
    reimpute(data, control)
  }

  lrn = makePreprocWrapper(learner, trainfun, predictfun, par.vals = args)
  lrn$id = stri_replace(lrn$id, replacement = ".imputed", regex = "[.]preproc$")
  addClasses(lrn, "ImputeWrapper")
}

#' @export
getLearnerProperties.ImputeWrapper = function(learner) {
  union(getLearnerProperties(learner$next.learner), "missings")
}
