#' @export
makeRLearner.regr.blm = function() {
  makeRLearnerRegr(
    cl = "regr.blm",
    package = "tgp",
    par.set = makeParamSet(
      makeDiscreteLearnerParam(id = "meanfn", default = "linear",
        values = c("constant", "linear")),
      makeDiscreteLearnerParam(id = "bprior", default = "bflat",
        values = c("b0", "b0not", "bflat", "bmle", "bmznot", "bmzt")),
      makeIntegerVectorLearnerParam(id = "BTE", len = 3,
        default = c(1000, 4000, 3), lower = 0),
      makeIntegerLearnerParam(id = "R", default = 1, lower = 1),
      makeLogicalLearnerParam(id = "m0r1", default = TRUE),
      makeUntypedLearnerParam(id = "itemps", default = NULL),
      makeLogicalLearnerParam(id = "krige", default = TRUE),
      makeLogicalLearnerParam(id = "zcov", default = FALSE),
      makeLogicalLearnerParam(id = "Ds2x", default = FALSE),
      makeLogicalLearnerParam(id = "improv", default = FALSE),
      makeLogicalLearnerParam(id = "trace", default = FALSE, tunable = FALSE),
      makeIntegerLearnerParam(id = "verb", default = 1L, lower = 0L, upper = 4L, tunable = FALSE)
    ),
    properties = c("numerics", "se"),
    name = "Bayesian Linear Model",
    short.name = "blm",
    callees = "blm"
  )
}

#' @export
trainLearner.regr.blm = function(.learner, .task, .subset, .weights = NULL, ...) {
  d = getTaskData(.task, .subset, target.extra = TRUE)
  tgp::blm(X = d$data, Z = d$target, pred.n = FALSE, ...)
}

#' @export
predictLearner.regr.blm = function(.learner, .model, .newdata, ...) {
  p = predict(.model$learner.model, XX = .newdata, pred.n = FALSE, ...)
  if (.learner$predict.type == "response") {
    return(p$ZZ.km)
  } else {
    return(cbind(p$ZZ.km, sqrt(p$ZZ.ks2)))
  }

}
