#' @export
makeRLearner.regr.btgpllm = function() {
  makeRLearnerRegr(
    cl = "regr.btgpllm",
    package = "tgp",
    par.set = makeParamSet(
      makeDiscreteLearnerParam(id = "meanfn", default = "linear",
        values = c("constant", "linear")),
      makeDiscreteLearnerParam(id = "bprior", default = "bflat",
        values = c("b0", "b0not", "bflat", "bmle", "bmznot", "bmzt")),
      makeDiscreteLearnerParam(id = "corr", default = "expsep",
        values = c("exp", "expsep", "matern", "sim")),
      makeNumericVectorLearnerParam(id = "tree", len = 2, default = c(0.5, 2),
        lower = c(0, 0), upper = c(1, Inf)),
      makeNumericVectorLearnerParam(id = "gamma", len = 3, default = c(10, 0.2, 0.7),
        lower = 0, upper = c(Inf, 1, 1)),
      makeIntegerVectorLearnerParam(id = "BTE", len = 3,
        default = c(2000, 7000, 2), lower = 0),
      makeIntegerLearnerParam(id = "R", default = 1, lower = 1),
      makeLogicalLearnerParam(id = "m0r1", default = TRUE),
      makeLogicalLearnerParam(id = "linburn", default = FALSE),
      makeUntypedLearnerParam(id = "itemps", default = NULL),
      makeLogicalLearnerParam(id = "krige", default = TRUE),
      makeLogicalLearnerParam(id = "zcov", default = FALSE),
      makeLogicalLearnerParam(id = "Ds2x", default = FALSE),
      makeLogicalLearnerParam(id = "improv", default = FALSE),
      makeNumericLearnerParam(id = "nu", default = 1.5,
        requires = quote(corr == "matern")),
      makeLogicalLearnerParam(id = "trace", default = FALSE, tunable = FALSE),
      makeIntegerLearnerParam(id = "verb", default = 1L, lower = 0L, upper = 4L, tunable = FALSE)
    ),
    properties = c("numerics", "se", "factors"),
    name = "Bayesian Treed Gaussian Process with jumps to the Limiting Linear Model",
    short.name = "btgpllm",
    callees = "btgpllm"
  )
}


#' @export
trainLearner.regr.btgpllm = function(.learner, .task, .subset, .weights = NULL, ...) {
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
    return(tgp::btgpllm(X = d$data, Z = d$target, basemax = n.num, pred.n = FALSE, ...))
  } else {
    return(tgp::btgpllm(X = d$data, Z = d$target, pred.n = FALSE, ...))
  }
}

#' @export
predictLearner.regr.btgpllm = function(.learner, .model, .newdata, ...) {
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
