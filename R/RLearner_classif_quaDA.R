#' @export
makeRLearner.classif.quaDA = function() {
  makeRLearnerClassif(
    cl = "classif.quaDA",
    package = "DiscriMiner",
    par.set = makeParamSet(
      #makeNumericVectorLearnerParam(id="prior", lower=0, upper=1, default=NULL),
      ),
    properties = c("twoclass", "multiclass", "numerics")
  )
}

#' @export
trainLearner.classif.quaDA = function(.learner, .task, .subset, .weights = NULL,  ...) {
  d = getTaskData(.task, .subset, target.extra=TRUE)
  is.prob = (.learner$predict.type == "prob")
  quaDA(variables = d$data, group = d$target, prob = is.prob)
}

#' @export
predictLearner.classif.quaDA = function(.learner, .model, .newdata, ...) {
  m = .model$learner.model
  p = classify(m, newdata = .newdata)
  #p$scores #we loose this information
  p$pred_class
}
