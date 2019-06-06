#' @export
makeRLearner.classif.sda = function() {
  makeRLearnerClassif(
    cl = "classif.sda",
    package = "sda",
    par.set = makeParamSet(
      makeNumericLearnerParam("lambda", lower = 0, upper = 1),
      makeNumericLearnerParam("lambda.var", lower = 0, upper = 1),
      makeNumericLearnerParam("lambda.freqs", lower = 0, upper = 1),
      makeLogicalLearnerParam("diagonal", default = FALSE),
      makeLogicalLearnerParam("verbose", default = TRUE, tunable = FALSE)
    ),
    properties = c("twoclass", "multiclass", "numerics", "prob"),
    name = "Shrinkage Discriminant Analysis",
    short.name = "sda",
    callees = "sda"
  )
}

#' @export
trainLearner.classif.sda = function(.learner, .task, .subset, ...) {
  d = getTaskData(.task, .subset, target.extra = TRUE)
  sda::sda(Xtrain = as.matrix(d$data), L = d$target, ...)
}

#' @export
predictLearner.classif.sda = function(.learner, .model, .newdata, ...) {
  p = sda::predict.sda(.model$learner.model, as.matrix(.newdata))
  if (.learner$predict.type == "response") {
    return(p$class)
  } else {
    return(p$posterior)
  }
}
