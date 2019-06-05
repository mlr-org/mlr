#' @export
makeRLearner.classif.mda = function() {
  makeRLearnerClassif(
    cl = "classif.mda",
    package = "!mda",
    # FIXME: stringdot pars and check order, scale and offset limits
    par.set = makeParamSet(
      makeUntypedLearnerParam(id = "subclasses", default = 2L),
      makeIntegerLearnerParam(id = "sub.df", lower = 1L),
      makeIntegerLearnerParam(id = "tot.df", lower = 1L),
      makeIntegerLearnerParam(id = "dimension", lower = 1L),
      makeNumericLearnerParam(id = "eps", default = .Machine$double.eps, lower = 0),
      makeIntegerLearnerParam(id = "iter", default = 5L, lower = 1L),
      makeDiscreteLearnerParam(id = "method", default = mda::polyreg,
        values = list(polyreg = mda::polyreg, mars = mda::mars, bruto = mda::bruto, gen.ridge = mda::gen.ridge)),
      makeLogicalLearnerParam(id = "keep.fitted", default = TRUE),
      makeLogicalLearnerParam(id = "trace", default = FALSE, tunable = FALSE),
      makeDiscreteLearnerParam(id = "start.method", default = "kmeans", values = c("kmeans", "lvq")),
      makeIntegerLearnerParam(id = "tries", default = 5L, lower = 1L),
      makeDiscreteLearnerParam(id = "criterion", default = "misclassification", values = c("misclassification", "deviance"))
    ),
    par.vals = list(keep.fitted = FALSE, start.method = "lvq"),
    properties = c("twoclass", "multiclass", "numerics", "factors", "prob"),
    name = "Mixture Discriminant Analysis",
    short.name = "mda",
    note = '`keep.fitted` has been set to `FALSE` by default for speed and we use `start.method = "lvq"` for more robust behavior / less technical crashes.',
    callees = c("mda", "mda.start")
  )
}

#' @export
trainLearner.classif.mda = function(.learner, .task, .subset, .weights = NULL, ...) {
  f = getTaskFormula(.task)
  mda::mda(f, data = getTaskData(.task, .subset), ...)
}

#' @export
predictLearner.classif.mda = function(.learner, .model, .newdata, ...) {
  type = ifelse(.learner$predict.type == "response", "class", "posterior")
  predict(.model$learner.model, newdata = .newdata, type = type, ...)
}
