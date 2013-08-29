makeRLearner.classif.sda = function() {
  makeRLearnerClassif(
    cl = "classif.sda",
    package = "sda",
    par.set = makeParamSet(
      makeLogicalLearnerParam(id="diagonal", default=FALSE)
    ), 
    twoclass = TRUE,
    multiclass = TRUE,
    numerics = TRUE,
    prob = TRUE
  )
}

trainLearner.classif.sda = function(.learner, .task, .subset,  ...) {
  d = getTaskData(.task, .subset, target.extra=TRUE)
  sda(Xtrain = as.matrix(d$data), L = d$target, ...)
}

predictLearner.classif.sda = function(.learner, .model, .newdata, ...) {
  p = predict(.model$learner.model, as.matrix(.newdata))
  if(.learner$predict.type == "response")
    return(p$class)
  else
    return(p$posterior)
}