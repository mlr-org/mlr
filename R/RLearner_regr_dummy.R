#' @title Dummy regression learner.
#'
#' @description
#' Very basic baseline method, mainly useful in model comparisons (if you don't beat this
#' you very likely have a problem).
#' Does not consider features in task, only looks at labels in training data.
#'
#' Methods \dQuote{mean} and \dQuote{median} always predict the mean / median of the training outputs
#' data for each new observation in the test set.
#'
#' The predict.type \dQuote{se} always used the standard deviation of the training outputs as se-estimator
#' for each new point in the test set.

#' @export
makeRLearner.regr.dummy = function() {
  makeRLearnerRegr(
    cl = "regr.dummy",
    package = "mlr",
    par.set = makeParamSet(
      makeDiscreteLearnerParam(id = "method", default = "mean", values = c("mean", "median"))
    ),
    # FIXME: check properties
    properties = c("numerics", "factors", "se"),
    name = "Featureless dummy predictor",
    short.name = "dummy",
    # FIXME: doc on page
    note = "Simple baseline meathod. Either always predicts the majority class in the training data or sampkles "
  )
}

#' @export
trainLearner.regr.dummy = function(.learner, .task, .subset, .weights = NULL,  ...) {
  y = getTaskTargets(.task)[.subset]
  # FIXME: use weights
  list(y = y)
}

#' @export
predictLearner.regr.dummy = function(.learner, .model, .newdata, ...) {
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


