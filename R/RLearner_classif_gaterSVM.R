#' @export
makeRLearner.classif.gaterSVM = function() {
  makeRLearnerClassif(
    cl = "classif.gaterSVM",
    package = "SwarmSVM",
    par.set = makeParamSet(
      makeIntegerLearnerParam(id = "m", default = 3, lower = 1),
      makeNumericLearnerParam(id = "c", default = 1, lower = 0),
      makeIntegerLearnerParam(id = "max.iter", default = 1, lower = 1),
      # FIXME m and max.iter seem to have no default in gaterSVM. If they have, par.vals is redundant.
      makeIntegerLearnerParam(id = "hidden", default = 5, lower = 0),
      makeNumericLearnerParam(id = "learningrate", default = 0.01, lower = 0),
      makeNumericLearnerParam(id = "threshold", default = 0.01, lower = 0),
      makeIntegerLearnerParam(id = "stepmax", default = 100, lower = 1),
      makeNumericLearnerParam(id = "seed"),
      makeUntypedLearnerParam(id = "valid.x", default = NULL),
      makeUntypedLearnerParam(id = "valid.y", default = NULL),
      makeUntypedLearnerParam(id = "valid.metric", default = NULL),
      makeLogicalLearnerParam(id = "verbose", default = FALSE)
    ),
    par.vals = list(m = 3, max.iter = 1),
    properties = c("twoclass", "numerics"),
    name = "Mixture of SVMs with Neural Network Gater Function",
    short.name = "gaterSVM",
    note = "`m` set to `3` and `max.iter` set to `1` by default.",
    callees = "gaterSVM"
  )
}

#' @export
trainLearner.classif.gaterSVM = function(.learner, .task, .subset, .weights = NULL, ...) {
  d = getTaskData(.task, .subset, target.extra = TRUE)
  SwarmSVM::gaterSVM(x = d$data, y = d$target, ...)
}

#' @export
predictLearner.classif.gaterSVM = function(.learner, .model, .newdata, ...) {
  factor(predict(.model$learner.model, newdata = .newdata, ...),
    levels = c(-1, 1),
    labels = .model$factor.levels[[1]])
}
