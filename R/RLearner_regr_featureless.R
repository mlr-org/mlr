#' @export
makeRLearner.regr.featureless = function() {
  makeRLearnerRegr(
    cl = "regr.featureless",
    package = "mlr",
    par.set = makeParamSet(
      makeDiscreteLearnerParam(id = "method", default = "mean", values = c("mean", "median"))
    ),
    properties = c("numerics", "factors", "ordered", "missings", "functionals"),
    name = "Featureless regression",
    short.name = "featureless"
  )
}

#' @export
trainLearner.regr.featureless = function(.learner, .task, .subset, .weights = NULL, method = "mean", ...) {
  y = getTaskTargets(.task)
  if (!is.null(.subset)) {
    y = y[.subset]
  }
  if (method == "mean") {
    response = mean(y)
  } else if (method == "median") {
    response = median(y)
  }

  list(response = response)
}

#' @export
predictLearner.regr.featureless = function(.learner, .model, .newdata, ...) {
  # extract some shortcuts
  n = nrow(.newdata)
  mod = getLearnerModel(.model)

  return(rep(mod$response, n))
}
