#' @export
makeRLearner.classif.mda = function() {
  makeRLearnerClassif(
    cl = "classif.mda",
    package = "mda",
    # FIXME: stringdot pars and check order, scale and offset limits
    par.set = makeParamSet(
      makeUntypedLearnerParam(id = "subclasses", default = 2L),
      makeIntegerLearnerParam(id = "iter", default = 5L, lower = 1L),
      makeIntegerLearnerParam(id = "dimension", lower = 1L),
      makeDiscreteLearnerParam(id = "method", default = mda::polyreg,
        values = list(polyreg = mda::polyreg, mars = mda::mars, bruto = mda::bruto, gen.ridge = mda::gen.ridge)),
      makeLogicalLearnerParam(id = "trace", default = FALSE),
      makeLogicalLearnerParam(id = "keep.fitted", default = TRUE),
      makeIntegerLearnerParam(id = "tries", default = 5L, lower = 1L)
    ),
    par.vals = list(keep.fitted = FALSE),
    properties = c("twoclass", "multiclass", "numerics", "factors", "prob"),
    name = "Mixture Discriminant Analysis",
    short.name = "mda",
    note = "`keep.fitted` has been set to `FALSE` by default for speed."
  )
}

#' @export
trainLearner.classif.mda = function(.learner, .task, .subset, .weights = NULL,  ...) {
  f = getTaskFormula(.task)
  mda::mda(f, data = getTaskData(.task, .subset), ...)
}

#' @export
predictLearner.classif.mda = function(.learner, .model, .newdata, ...) {
  type = ifelse(.learner$predict.type=="response", "class", "posterior")
  predict(.model$learner.model, newdata = .newdata, type = type, ...)
}

