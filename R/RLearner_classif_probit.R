#' @export
makeRLearner.classif.probit = function() {
  makeRLearnerClassif(
    cl = "classif.probit",
    package = "stats",
    par.set = makeParamSet(
      makeLogicalLearnerParam("model", default = TRUE, tunable = FALSE)
    ),
    par.vals = list(
      model = FALSE
    ),
    properties = c("twoclass", "numerics", "factors", "prob", "weights"),
    name = "Probit Regression",
    short.name = "probit",
    note = "Delegates to `glm` with `family = binomial(link = 'probit')`. We set 'model' to FALSE by default to save memory.",
    callees = "glm"
  )
}

#' @export
trainLearner.classif.probit = function(.learner, .task, .subset, .weights = NULL, ...) {
  f = getTaskFormula(.task)
  stats::glm(f, data = getTaskData(.task, .subset),
    family = binomial(link = "probit"), weights = .weights, ...)
}

#' @export
predictLearner.classif.probit = function(.learner, .model, .newdata, ...) {
  x = predict(.model$learner.model, newdata = .newdata, type = "response", ...)
  levs = .model$task.desc$class.levels
  if (.learner$predict.type == "prob") {
    propVectorToMatrix(x, levs)
  } else {
    levs = .model$task.desc$class.levels
    p = as.factor(ifelse(x > 0.5, levs[2L], levs[1L]))
    unname(p)
  }
}
