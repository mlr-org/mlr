#' @export
makeRLearner.classif.logreg = function() {
  makeRLearnerClassif(
    cl = "classif.logreg",
    package = "stats",
    par.set = makeParamSet(),
    properties = c("twoclass", "multiclass", "numerics", "factors", "prob", "weights"),
    name = "classif.logreg",
    short.name = "logreg",
    note = ""
  )
}

#' @export
trainLearner.classif.logreg = function(.learner, .task, .subset, .weights = NULL,  ...) {
  f = getTaskFormula(.task)
  glm(f, data = getTaskData(.task, .subset), model = FALSE, family = "binomial", ...)
}

#' @export
predictLearner.classif.logreg = function(.learner, .model, .newdata, ...) {
  x = predict(.model$learner.model, newdata = .newdata, type = "response", ...)
  levs = .model$task.desc$class.levels
  if (.learner$predict.type == "prob") {
    # FIXME this should be a helper function
    y = matrix(0, ncol = 2L, nrow = nrow(.newdata))
    colnames(y) = levs
    y[,1L] = 1-x
    y[,2L] = x
    return(y)
  } else {
    levs = .model$task.desc$class.levels
    p = as.factor(ifelse(x > 0.5, levs[2L], levs[1L]))
    unname(p)
  }
}
