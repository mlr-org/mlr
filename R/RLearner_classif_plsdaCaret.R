#' @export
makeRLearner.classif.plsdaCaret = function() {
  makeRLearnerClassif(cl = "classif.plsdaCaret",
    package = "caret",
    par.set = makeParamSet(
      makeIntegerLearnerParam(id = "ncomp", default = 2, lower = 1),
      makeDiscreteLearnerParam(id = "probMethod", values = c("softmax", "Bayes"), default = "softmax")
    ),
    properties = c("numerics", "prob", "twoclass"),
    name = "Partial Least Squares (PLS) Discriminant Analysis",
    short.name = "plsdacaret",
    note = ""
  )
}

#' @export
trainLearner.classif.plsdaCaret = function(.learner, .task, .subset, .weights, ...) {
  d = getTaskData(.task, .subset, target.extra = TRUE)
  caret::plsda(d$data, d$target, method = "oscorespls", ...)
}

#' @export
predictLearner.classif.plsdaCaret = function(.learner, .model, .newdata, ...) {
  type = ifelse(.learner$predict.type == "response", "class", "prob")
  p = predict(.model$learner.model, newdata = .newdata, type = type, ...)
  if (type == "prob"){
    p = p[,,1]
  }
  return(p)
}
