#' @title Featureless regression learner.
#'
#' @description
#' A very basic baseline method which is useful for model comparisons (if you
#' don't beat this, you very likely have a problem).
#' Does not consider any features of the task and only uses the target feature
#' of the training data to make predictions.
#' Using observation weights is currently not supported.
#'
#' Methods \dQuote{mean} and \dQuote{median} always predict a constant value
#' for each new observation which corresponds to the observed mean or median of
#' the target feature in training data, respectively.
#'
#' The default method is \dQuote{mean} which corresponds to the ZeroR algorithm
#' from WEKA, see <https://weka.wikispaces.com/ZeroR>.
#'
#' @name regr.featureless
NULL

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
