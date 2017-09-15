#' @export
makeRLearner.classif.binomial = function() {
  makeRLearnerClassif(
    cl = "classif.binomial",
    package = "stats",
    par.set = makeParamSet(
      makeDiscreteLearnerParam("link", values = c("logit", "probit", "cloglog", "cauchit", "log"),
        default = "logit"),
      makeLogicalLearnerParam("model", default = TRUE, tunable = FALSE)
    ),
    par.vals = list(
      model = FALSE
    ),
    properties = c("twoclass", "numerics", "factors", "prob", "weights"),
    name = "Binomial Regression",
    short.name = "binomial",
    note = "Delegates to `glm` with freely choosable binomial link function via learner parameter `link`. We set 'model' to FALSE by default to save memory.",
    callees = c("glm", "binomial")
  )
}

#' @export
trainLearner.classif.binomial = function(.learner, .task, .subset, .weights = NULL, link = "logit", ...) {
  f = getTaskFormula(.task)
  stats::glm(f, data = getTaskData(.task, .subset), family = stats::binomial(link = link), weights = .weights, ...)
}

#' @export
predictLearner.classif.binomial = function(.learner, .model, .newdata, ...) {

  # set factor levels, present in test but missing in train, to NA
  if (.model$learner$fix.factors.prediction == TRUE &&
      any(vlapply(.newdata, function(x) is.factor(x)))) {

    # cheap error catching here
    # in @test_base_generateFilterValuesData.R#93 data is not stored in m$learner.model ??
    if (is.null(subset)) {
      .newdata = .model$learner.model$data[subset, ]
    }
    .newdata = missingLevelsTrain(.model, .newdata)
  }

  x = predict(.model$learner.model, newdata = .newdata, type = "response", ...)
  levs = .model$task.desc$class.levels
  if (.learner$predict.type == "prob") {
    propVectorToMatrix(x, levs)
  } else {
    levs = .model$task.desc$class.levels
    p = as.factor(ifelse(x > 0.5, levs[2L], levs[1L]))
    unname(p)
  }
}


