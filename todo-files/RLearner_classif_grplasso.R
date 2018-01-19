
makeRLearner.classif.grplasso = function() {
  makeRLearnerClassif(
    cl = "classif.grplasso",
    package = "grplasso",
    par.set = makeParamSet(
      makeNumericLearnerParam(id="lambda", default=1, lower=0),
      makeUntypedLearnerParam(id="index")
    ), 
    twoclass = TRUE,
    numerics = TRUE,
    prob = TRUE,
    weights = TRUE
  )
}

trainLearner.classif.grplasso = function(.learner, .task, .subset,  ...) {
  # FIXME: bug in grplasso: index cant be passed with formula interface....
  d = getTaskData(.task, .subset, target.extra=TRUE, recode.target="01")
  x = cbind(1, as.matrix(d$data))
  if (.task$task.desc$has.weights)
    grplasso(x, d$target, weights=.task$weights[.subset], ...)
  else
    grplasso(x, d$target, ...)
}

predictLearner.classif.grplasso = function(.learner, .model, .newdata, ...) {
  x = as.matrix(.newdata)
  x = cbind(1, x)
  p = as.numeric(predict(.model$learner.model, newdata=x, type="response", ...))
  levs = c(.model$task.desc$negative, .model$task.desc$positive)    
  if (.learner$predict.type == "prob") {
    y <- matrix(0, ncol=2, nrow=nrow(.newdata))
    colnames(y) = levs
    y[,1] = 1-p
    y[,2] = p
    return(y)
  } else {
    p = as.factor(ifelse(p > 0.5, levs[2], levs[1]))
    names(p) = NULL
    return(p)
  }
}



