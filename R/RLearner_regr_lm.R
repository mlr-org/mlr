#' @export
makeRLearner.regr.lm = function() {
  makeRLearnerRegr(
    cl = "regr.lm",
    package = "stats",
    par.set = makeParamSet(
      makeNumericLearnerParam(id = "tol", default = 1e-7, lower = 0),
      makeLogicalLearnerParam(id = "singular.ok", default = TRUE, tunable = FALSE)
    ),
    properties = c("numerics", "factors", "se", "weights"),
    name = "Simple Linear Regression",
    short.name = "lm",
    callees = c("lm", "lm.fit", "lm.wfit")
  )
}

#' @export
trainLearner.regr.lm = function(.learner, .task, .subset, .weights = NULL,  ...) {
  d = getTaskData(.task, .subset)
  if (is.null(.weights)) {
    f = getTaskFormula(.task)
    stats::lm(f, data = d, ...)
  } else  {
    f = getTaskFormula(.task)
    stats::lm(f, data = d, weights = .weights, ...)
  }
}

#' @export
predictLearner.regr.lm = function(.learner, .model, .newdata, ...) {

  # set factor levels, present in test but missing in train, to NA
  if (.model$learner$fix.factors.prediction == TRUE &&
      any(vlapply(.newdata, function(x) is.factor(x)))) {
    # sometimes we have no task here, e.g. in test_tune_tuneMBO@34
    if (!missing(task)) {
      .newdata = task$env$data[subset, ]
    } else {
      .newdata = .newdata
    }

    # cheap error catching here
    # in @test_base_generateFilterValuesData.R#93 data is not stored in m$learner.model ??
    if (is.null(subset)) {
      .newdata = model$learner.model$data[subset, ]
    }
    .newdata = missingLevelsTrain(.model, .newdata)
  }
  if (.learner$predict.type == "response") {
    predict(.model$learner.model, newdata = .newdata, se.fit = FALSE, ...)
  } else {
    p = predict(.model$learner.model, newdata = .newdata, se.fit = TRUE, ...)
    cbind(p$fit, p$se.fit)
  }
}
