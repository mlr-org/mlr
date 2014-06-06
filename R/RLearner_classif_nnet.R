#' @export
makeRLearner.classif.nnet = function() {
  makeRLearnerClassif(
    cl = "classif.nnet",
    package = "nnet",
    par.set = makeParamSet(
      makeIntegerLearnerParam(id="size", default=3L, lower=0L),
      makeIntegerLearnerParam(id="maxit", default=100L, lower=1L),
      # nnet seems to set these manually and hard for classification.....
#     makeLogicalLearnerParam(id="linout", default=FALSE, requires=expression(entropy==FALSE && softmax==FALSE && censored==FALSE)),
#     makeLogicalLearnerParam(id="entropy", default=FALSE, requires=expression(linout==FALSE && softmax==FALSE && censored==FALSE)),
#     makeLogicalLearnerParam(id="softmax", default=FALSE, requires=expression(entropy==FALSE && linout==FALSE && censored==FALSE)),
#     makeLogicalLearnerParam(id="censored", default=FALSE, requires=expression(linout==FALSE && softmax==FALSE && entropy==FALSE)),
      makeLogicalLearnerParam(id="skip", default=FALSE),
      makeNumericLearnerParam(id="rang", default=0.7),
      makeNumericLearnerParam(id="decay", default=0),
      makeLogicalLearnerParam(id="Hess", default=FALSE),
      makeLogicalLearnerParam(id="trace", default=TRUE),
      makeIntegerLearnerParam(id="MaxNWts", default=1000L, lower=1L),
      makeNumericLearnerParam(id="abstoll", default=1.0e-4),
      makeNumericLearnerParam(id="reltoll", default=1.0e-8)
    ),
    par.vals = list(size=3L),
    properties = c("twoclass", "multiclass", "numerics", "factors", "prob", "weights")
  )
}

#' @export
trainLearner.classif.nnet = function(.learner, .task, .subset, .weights = NULL,  ...) {
  if (is.null(.weights)) {
    f = getTaskFormula(.task)
    nnet(f, data=getTaskData(.task, .subset), ...)
  } else  {
    f = as.formula(getTaskFormulaAsString(.task))
    nnet(f, data=getTaskData(.task, .subset), weights=.weights, ...)
  }
}

#' @export
predictLearner.classif.nnet = function(.learner, .model, .newdata, ...) {
  type = switch(.learner$predict.type, response="class", prob="raw")
  p = predict(.model$learner.model, newdata=.newdata, type=type, ...)
  if (type == "class")
    return(as.factor(p))
  else {
    if (length(.model$task.desc$class.levels) == 2L) {
      y <- cbind(1-p, p)
      colnames(y) = .model$learner.model$lev
      return(y)
    } else
      return(p)
  }
}
