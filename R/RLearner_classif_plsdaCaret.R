#' @export
makeRLearner.classif.plsdaCaret = function() {
  makeRLearnerClassif(cl = "classif.plsdaCaret",
    package = c("caret", "pls"),
    par.set = makeParamSet(
      makeIntegerLearnerParam(id = "ncomp", default = 2, lower = 1),
      makeDiscreteLearnerParam(id = "probMethod", values = c("softmax", "Bayes"), default = "softmax"),
      makeDiscreteLearnerParam(id = "method", default = "kernelpls",
        values = c("kernelpls", "widekernelpls", "simpls", "oscorespls"))
    ),
    properties = c("numerics", "prob", "twoclass", "multiclass"),
    name = "Partial Least Squares (PLS) Discriminant Analysis",
    short.name = "plsdacaret",
    callees = c("plsda", "plsr")
  )
}

#' @export
trainLearner.classif.plsdaCaret = function(.learner, .task, .subset, .weights, ...) {
  d = getTaskData(.task, .subset, target.extra = TRUE)
  caret::plsda(d$data, d$target, ...)
}

#' @export
predictLearner.classif.plsdaCaret = function(.learner, .model, .newdata, ...) {
  type = ifelse(.learner$predict.type == "response", "class", "prob")
  p = predict(.model$learner.model, newdata = .newdata, type = type, ...)
  if (type == "prob") {
    p = array(c(p), dim(p)[-3], dimnames = dimnames(p)[1:2])
  }
  return(p)
}
