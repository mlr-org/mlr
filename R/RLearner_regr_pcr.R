#' @S3method makeRLearner regr.pcr
makeRLearner.regr.pcr = function() {
  makeRLearnerRegr(
    cl = "regr.pcr",
    package = "pls",
    par.set = makeParamSet(
      makeIntegerLearnerParam(id="ncomp", lower=1L),
      makeDiscreteLearnerParam(id="method", default="cppls",
        values=c("kernelpls", "widekernelpls", "simpls", "oscorespls", "cppls", "svdpc")),
      makeLogicalLearnerParam(id="scale", default=FALSE),
      makeLogicalLearnerParam(id="model", default=TRUE),
      makeLogicalLearnerParam(id="x", default=FALSE),
      makeLogicalLearnerParam(id="y", default=FALSE)
    ),
    par.vals = list(model=FALSE),
    missings = FALSE,
    numerics = TRUE,
    factors = TRUE,
    se = FALSE,
    weights = FALSE
  )
}

#' @S3method trainLearner regr.pcr
trainLearner.regr.pcr = function(.learner, .task, .subset, .weights,  ...) {
  f = getTaskFormula(.task)
  pcr(f, data=getTaskData(.task, .subset), ...)
}

#' @S3method predictLearner regr.pcr
predictLearner.regr.pcr = function(.learner, .model, .newdata, ...) {
  p = predict(.model$learner.model, newdata=.newdata)
  p[, 1L, dim(p)[3L]]
}
