#' @S3method makeRLearner regr.kknn
makeRLearner.regr.kknn = function() {
  makeRLearnerRegr(
    cl = "regr.kknn",
    package = "kknn",
    par.set = makeParamSet(
      makeIntegerLearnerParam(id="k", default=7L, lower=1L),
      makeNumericLearnerParam(id="distance", default=2, lower=0),
      makeDiscreteLearnerParam(id="kernel", default="triangular", 
        values=list("rectangular", "triangular", "epanechnikov", "biweight", "triweight", "cos", "inv", "gaussian"))
    ), 
    missings = FALSE,
    numerics = TRUE,
    factors = TRUE,
    se = FALSE,
    weights = FALSE
  )
}

#' @S3method trainLearner regr.kknn
trainLearner.regr.kknn = function(.learner, .task, .subset, .weights,  ...) {
  list(td=.task$task.desc, data=getTaskData(.task, .subset), parset=list(...))
}

#' @S3method predictLearner regr.kknn
predictLearner.regr.kknn = function(.learner, .model, .newdata, ...) {
  m = .model$learner.model
  f = getTaskFormula(.model$task.desc)
  pars = list(formula=f, train=m$data, test=.newdata)  
  pars = c(pars, m$parset, list(...))
  m = do.call(kknn, pars)
  return(m$fitted.values)
}