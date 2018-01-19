#' @export
makeRLearner.classif.nnet = function() {
  makeRLearnerClassif(
    cl = "classif.nnet",
    package = "nnet",
    par.set = makeParamSet(
      makeIntegerLearnerParam(id = "size", default = 3L, lower = 0L),
      # FIXME size seems to have no default in nnet(). If it has, par.vals is redundant
      makeIntegerLearnerParam(id = "maxit", default = 100L, lower = 1L),
      # nnet seems to set these manually and hard for classification.....
#     makeLogicalLearnerParam(id = "linout", default = FALSE, requires = quote(entropy == FALSE && softmax == FALSE && censored == FALSE)),
#     makeLogicalLearnerParam(id = "entropy", default = FALSE, requires = quote(linout == FALSE && softmax == FALSE && censored == FALSE)),
#     makeLogicalLearnerParam(id = "softmax", default = FALSE, requires = quote(entropy == FALSE && linout == FALSE && censored == FALSE)),
#     makeLogicalLearnerParam(id = "censored", default = FALSE, requires = quote(linout == FALSE && softmax == FALSE && entropy == FALSE)),
      makeLogicalLearnerParam(id = "skip", default = FALSE),
      makeNumericLearnerParam(id = "rang", default = 0.7),
      makeNumericLearnerParam(id = "decay", default = 0),
      makeLogicalLearnerParam(id = "Hess", default = FALSE),
      makeLogicalLearnerParam(id = "trace", default = TRUE, tunable = FALSE),
      makeIntegerLearnerParam(id = "MaxNWts", default = 1000L, lower = 1L, tunable = FALSE),
      makeNumericLearnerParam(id = "abstol", default = 1.0e-4),
      makeNumericLearnerParam(id = "reltol", default = 1.0e-8)
    ),
    par.vals = list(size = 3L),
    properties = c("twoclass", "multiclass", "numerics", "factors", "prob", "weights"),
    name = "Neural Network",
    short.name = "nnet",
    note = "`size` has been set to `3` by default.",
    callees = "nnet"
  )
}

#' @export
trainLearner.classif.nnet = function(.learner, .task, .subset, .weights = NULL,  ...) {
  if (is.null(.weights)) {
    f = getTaskFormula(.task)
    nnet::nnet(f, data = getTaskData(.task, .subset), ...)
  } else  {
    f = getTaskFormula(.task)
    nnet::nnet(f, data = getTaskData(.task, .subset), weights = .weights, ...)
  }
}

#' @export
predictLearner.classif.nnet = function(.learner, .model, .newdata, ...) {
  type = switch(.learner$predict.type, response = "class", prob = "raw")
  p = predict(.model$learner.model, newdata = .newdata, type = type, ...)
  if (type == "class")
    return(as.factor(p))
  else {
    if (length(.model$task.desc$class.levels) == 2L) {
      y = cbind(1 - p, p)
      colnames(y) = .model$learner.model$lev
      return(y)
    } else
      return(p)
  }
}
