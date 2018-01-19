#' @export
makeRLearner.classif.wsrf = function() {
  makeRLearnerClassif(
    cl = "classif.wsrf",
    package = "wsrf",
    par.set = makeParamSet(
      makeIntegerLearnerParam(id = "mtry", lower = 1L),
      makeIntegerLearnerParam(id = "ntrees", default = 500L, lower = 1L),
      makeLogicalLearnerParam(id = "weights", default = TRUE),
      makeLogicalLearnerParam(id = "parallel", default = FALSE)
    ),
    par.vals = list(parallel = FALSE),
    properties = c("twoclass", "multiclass", "numerics", "factors", "ordered", "prob"),
    name = "Weighted Subspace Random Forest",
    short.name = "wsrf",
    note = "`parallel` has been set to `FALSE` by default."
  )
}

#' @export
trainLearner.classif.wsrf = function(.learner, .task, .subset, .weights = NULL, ...) {
  f = getTaskFormula(.task)
  wsrf::wsrf(f, data = getTaskData(.task, .subset), ...)
}

#' @export
predictLearner.classif.wsrf = function(.learner, .model, .newdata, ...) {
  p = wsrf::predict.wsrf(.model$learner.model, .newdata, type = .learner$predict.type)
  if (.learner$predict.type == "response")
    return(p)
  else
    return(as.matrix(p))
}
