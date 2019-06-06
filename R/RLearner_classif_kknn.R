#' @export
makeRLearner.classif.kknn = function() {
  makeRLearnerClassif(
    cl = "classif.kknn",
    # FIXME: kknn set its own contr.dummy function, if we requireNamespace,
    # this is not found, see issue  226
    package = "!kknn",
    # FIXME: find out what ykernel and contrasts really do
    par.set = makeParamSet(
      makeIntegerLearnerParam(id = "k", default = 7L, lower = 1L),
      makeNumericLearnerParam(id = "distance", default = 2, lower = 0),
      makeDiscreteLearnerParam(id = "kernel", default = "optimal",
        values = list("rectangular", "triangular", "epanechnikov", "biweight",
          "triweight", "cos", "inv", "gaussian", "optimal")),
      makeLogicalLearnerParam(id = "scale", default = TRUE)
    ),
    properties = c("twoclass", "multiclass", "numerics", "factors", "prob"),
    name = "k-Nearest Neighbor",
    short.name = "kknn",
    callees = "kknn"
  )
}

#' @export
trainLearner.classif.kknn = function(.learner, .task, .subset, .weights = NULL, ...) {
  list(td = getTaskDesc(.task), data = getTaskData(.task, .subset), parset = list(...))
}

#' @export
predictLearner.classif.kknn = function(.learner, .model, .newdata, ...) {
  m = .model$learner.model
  f = getTaskFormula(.model$task.desc)
  pars = list(formula = f, train = m$data, test = .newdata)
  pars = c(pars, m$parset, list(...))
  m = do.call(kknn::kknn, pars)
  if (.learner$predict.type == "response") {
    return(m$fitted.values)
  } else {
    return(m$prob)
  }
}
