#' @S3method makeRLearner regr.rsm
makeRLearner.regr.rsm = function() {
  makeRLearnerRegr(
    cl = "regr.rsm",
    package = "rsm",
    par.set = makeParamSet(
      makeDiscreteLearnerParam(id="modelfun", default="FO", values=c("FO", "TWI", "SO"))
    ),
    par.vals = list(modelfun="FO"),
    missings = FALSE,
    numerics = TRUE,
    factors = FALSE,
    se = FALSE,
    weights = FALSE
  )
}

#' @S3method trainLearner regr.rsm
trainLearner.regr.rsm = function(.learner, .task, .subset, .weights,  ...) {
  mf = list(...)$modelfun
  vs = paste(getTaskFeatureNames(.task), collapse=",")
  g = function(x) paste(x, "(", vs, ")", sep="")
  mf = switch(mf,
    FO = g("FO"),
    TWI = paste(g("TWI"), "+", g("FO")),
    SO = g("SO"),
    stop("Unknown modelfun: ", mf)
  )
  f = as.formula(paste(.task$task.desc$target, "~", mf))
  myargs = list(f, getTaskData(.task, .subset))
  # strange behaviour in rsm forces us to use do.call...
  do.call(rsm, myargs)
}

#' @S3method predictLearner regr.rsm
predictLearner.regr.rsm = function(.learner, .model, .newdata, ...) {
  as.numeric(predict(.model$learner.model, newdata=.newdata, ...))
}
