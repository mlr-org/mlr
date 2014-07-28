#' @export
makeRLearner.classif.plsDA = function() {
  makeRLearnerClassif(
    cl = "classif.plsDA",
    package = "DiscriMiner",
    par.set = makeParamSet(
      makeLogicalLearnerParam(id = "autosel", default = TRUE),
      makeIntegerLearnerParam(id = "comps", lower = 1L, default = 2L, requires = expression(autosel==TRUE)),
      makeLogicalLearnerParam(id = "retain.models", default = FALSE)
      ),
    properties = c("twoclass", "multiclass", "numerics"),
    name = "classif.plsDA",
    short.name = "classif.plsDA",
    note = ""
  )
}

#' @export
trainLearner.classif.plsDA = function(.learner, .task, .subset, .weights = NULL,  ...) {
  d = getTaskData(.task, .subset, target.extra = TRUE)
  plsDA(variables = d$data, group = d$target, ...)
}

#' @export
predictLearner.classif.plsDA = function(.learner, .model, .newdata, ...) {
  m = .model$learner.model
  p = classify(m, newdata = .newdata)
  #p$scores #we loose this information
  p$pred_class
}
