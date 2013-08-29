makeRLearner.classif.llr = function() {
  makeRLearnerClassif(
    cl = "classif.llr",
    package = "locCLass",
    oneclass = FALSE,
    twoclass = TRUE,
    multiclass = FALSE,
    missings = FALSE,
    numerics = TRUE,
    factors = TRUE,
    prob = TRUE,
    weights = FALSE
  )
}

trainLearner.classif.llr = function(.learner, .task, .subset,  ...) {
  f = getTaskFormula(.task)
  llr(f, data=getTaskData(.task, .subset), ...)
}

predictLearner.classif.llr = function(.learner, .model, .newdata, ...) {
  p = predict(.model$learner.model, newdata=.newdata, ...)
  if(.learner$predict.type == "response")
    return(p$class)
  else
    return(p$posterior)
}