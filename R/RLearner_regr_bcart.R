#' @export
makeRLearner.regr.bcart = function() {
  makeRLearnerRegr(
    cl = "regr.bcart",
    package = "tgp",
    par.set = makeParamSet(
      makeDiscreteLearnerParam(id = "bprior", default = "bflat",
        values = c("b0", "b0not", "bflat", "bmle", "bmznot", "bmzt")),
      makeNumericVectorLearnerParam(id = "tree", len = 2, default = c(0.5, 2),
        lower = c(0, 0), upper = c(1, Inf)),
      makeIntegerVectorLearnerParam(id = "BTE", len = 3,
        default = c(2000, 7000, 2), lower = 0),
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
    properties = c("numerics", "se", "factors"),
    name = "Bayesian CART",
    short.name = "bcart",
    callees = "bcart"
  )
}

#' @export
trainLearner.regr.bcart = function(.learner, .task, .subset, .weights = NULL, ...) {
  d = getTaskData(.task, .subset, target.extra = TRUE)
  # factor variables must be in the last columns as dummy variables:
  col.types = vcapply(d$data, function(x) class(x))
  factor.ind = (col.types == "factor")
  if (any(factor.ind)) {
    d.num = d$data[, !factor.ind, drop = FALSE]
    n.num = ncol(d.num)
    d.factor = d$data[, factor.ind, drop = FALSE]
    d.factor = createDummyFeatures(d.factor, method = "reference")
    d$data = cbind(d.num, d.factor)
    return(tgp::bcart(X = d$data, Z = d$target, basemax = n.num, pred.n = FALSE, ...))
  } else {
    return(tgp::bcart(X = d$data, Z = d$target, pred.n = FALSE, ...))
  }
}

#' @export
predictLearner.regr.bcart = function(.learner, .model, .newdata, ...) {
  # factor variables must be in the last columns as dummy variables:
  col.types = vcapply(.newdata, function(x) class(x))
  factor.ind = (col.types == "factor")
  if (any(factor.ind)) {
    newdata.num = .newdata[, !factor.ind, drop = FALSE]
    newdata.factor = .newdata[, factor.ind, drop = FALSE]
    newdata.factor = createDummyFeatures(newdata.factor, method = "reference")
    newdata = cbind(newdata.num, newdata.factor)
    p = predict(.model$learner.model, XX = newdata, pred.n = FALSE, ...)
  } else {
    p = predict(.model$learner.model, XX = .newdata, pred.n = FALSE, ...)
  }
  if (.learner$predict.type == "response") {
    return(p$ZZ.km)
  } else {
    return(cbind(p$ZZ.km, sqrt(p$ZZ.ks2)))
  }
}
