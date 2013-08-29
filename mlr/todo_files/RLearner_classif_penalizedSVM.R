makeRLearner.classif.penalizedSVM = function() {
  makeRLearnerClassif(
    cl = "classif.penalizedSVM",
    package = "penalizedSVM",
    par.set = makeParamSet(
      makeDiscreteLearnerParam(id="fs.method", default="scad", values=c("scad","1norm", "DrHSVM", "scad+L2")),
      makeNumericLearnerParam(id="maxevals", default=500L),
      makeLogicalLearnerParam(id="calc.class.weights", default=FALSE),
      makeNumericLearnerParam(id="lambda1", lower=0),
      makeNumericLearnerParam(id="lambda2", lower=0)
    ), 
    twoclass = TRUE,
    numerics = TRUE
  )
}

trainLearner.classif.penalizedSVM = function(.learner, .task, .subset,  ...) {
  d = getTaskData(.task, .subset, target.extra=TRUE, recode.target="-1+1")
  svm.fs(x=as.matrix(d$data), y=d$target, verbose=FALSE, grid.search="discrete", parms.coding="none",
    lambda1.set=2, lambda2.set=2, inner.val.method="cv", cross.inner=2,
    set.seed=as.integer(runif(1, min=-.Machine$integer.max, max=.Machine$integer.max)))
}

predictLearner.classif.penalizedSVM = function(.learner, .model, .newdata, ...) {
  type = ifelse(.learner$predict.type == "response", "response", "probabilities")
  predict(.model$learner.model, newdata=.newdata, type=type, ...)
}
