#' @export
makeRLearner.regr.laGP = function() {
  makeRLearnerRegr(
    cl = "regr.laGP",
    package = "laGP",
    par.set = makeParamSet(
      makeIntegerLearnerParam(id = "start", default = 6, lower = 6),
      makeIntegerLearnerParam(id = "end", default = 50), # > start?
      makeUntypedLearnerParam(id = "d", default = NULL),
      makeUntypedLearnerParam(id = "g", default = 1 / 10000), # list?
      makeDiscreteLearnerParam(id = "method", default = "alc",
        values = c("alc", "alcray", "efi", "mspe", "nn")),
      # FIXME: default values of "close" and "numrays" depend on the size of
      # X, so the are not given here
      makeIntegerLearnerParam(id = "close", lower = 0),
      makeIntegerLearnerParam(id = "numrays", lower = 0,
        requires = quote(method == "alcray")),
      makeNumericLearnerParam(id = "verb", default = 0, tunable = FALSE)
    ),
    properties = c("numerics", "se"),
    name = "Local Approximate Gaussian Process",
    short.name = "laGP",
    callees = "aGP"
  )
}

#' @export
trainLearner.regr.laGP = function(.learner, .task, .subset, .weights = NULL, ...) {
  d = getTaskData(.task, .subset, target.extra = TRUE)
  return(list(data = d$data, target = d$target, parset = list(...)))
}

#' @export
predictLearner.regr.laGP = function(.learner, .model, .newdata, ...) {
  m = .model$learner.model
  pars = c(list(X = m$data, Z = m$target, XX = .newdata), Xi.ret = FALSE,
    m$parset, list(...))
  p = do.call(laGP::aGP, pars)
  if (.learner$predict.type == "response") {
    return(p$mean)
  } else {
    return(cbind(p$mean, sqrt(p$var)))
  }
}
