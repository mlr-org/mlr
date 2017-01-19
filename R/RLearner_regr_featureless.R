#' @title Featureless regression learner.
#'
#' @description
#' Very basic baseline method, mainly useful in model comparisons (if you don't
#' beat this you very likely have a problem).
#' Does not consider features of the task and only looks at labels on training
#' data.
#'
#' Methods \dQuote{mean} and \dQuote{median} always predict a constant value
#' for each new observation in the test set which is the mean or median of the
#' training data, respectively.
#'
#' The predict.type \dQuote{se} always uses the standard deviation of the
#' training data as se-estimator for each new point in the test set.
#'
#' @export
makeRLearner.regr.featureless = function() {
  makeRLearnerRegr(
    cl = "regr.featureless",
    package = "mlr",
    par.set = makeParamSet(
      makeDiscreteLearnerParam(id = "method", default = "mean", values = c("mean", "median"))
    ),
    properties = c("numerics", "factors", "ordered", "missings", "se"),
    name = "Featureless regression",
    short.name = "featureless",
    # FIXME: doc on page
    note = "Simple baseline method. Always predicts a constant value for each new observation in the test set which is the mean or median of the training data, respectively."
  )
}

#' @export
trainLearner.regr.featureless = function(.learner, .task, .subset, .weights = NULL,  ...) {
  y = getTaskTargets(.task)[.subset]
  # FIXME: use weights
  list(y = y)
}

#' @export
predictLearner.regr.featureless = function(.learner, .model, .newdata, ...) {
  # extract some shortcuts
  n = nrow(.newdata)
  ptype = .learner$predict.type
  y = .model$learner.model$y
  method = .learner$par.vals$method

  if (method == "mean") {
    resp = mean(y)
  } else if (method == "median") {
    resp = median(y)
  }

  if (ptype == "response") {
    return(rep(resp, n))
  } else {
    se = sd(y)
    return(matrix(c(resp, se), nrow = n, ncol = 2L, byrow = TRUE))
  }
}
