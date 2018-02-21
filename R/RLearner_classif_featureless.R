#' @title Featureless classification learner.
#'
#' @description
#' A very basic baseline method which is useful for model comparisons (if you
#' don't beat this, you very likely have a problem).
#' Does not consider any features of the task and only uses the target feature
#' of the training data to make predictions.
#' Using observation weights is currently not supported.
#'
#' Method \dQuote{majority} predicts always the majority class for each new
#' observation. In the case of ties, one randomly sampled, constant class is predicted
#' for all observations in the test set.
#' This method is used as the default. It is very similar to the ZeroR classifier
#' from WEKA (see <https://weka.wikispaces.com/ZeroR>). The only difference is
#' that ZeroR always predicts the first class of the tied class values instead
#' of sampling them randomly.
#'
#' Method \dQuote{sample-prior} always samples a random class for each individual test
#' observation according to the prior probabilities observed in the training data.
#'
#' If you opt to predict probabilities, the class probabilities always
#' correspond to the prior probabilities observed in the training data.
#'
#' @name classif.featureless
NULL

#' @export
makeRLearner.classif.featureless = function() {
  makeRLearnerClassif(
    cl = "classif.featureless",
    package = "mlr",
    par.set = makeParamSet(
      makeDiscreteLearnerParam(id = "method", default = "majority", values = c("majority", "sample-prior"))
    ),
    properties = c("twoclass", "multiclass", "numerics", "factors", "ordered", "missings", "prob", "functionals"),
    name = "Featureless classifier",
    short.name = "featureless"
  )
}

#' @export
trainLearner.classif.featureless = function(.learner, .task, .subset, .weights = NULL,
  method = "majority", ...) {
  y = getTaskTargets(.task)
  if (!is.null(.subset)) {
    y = y[.subset]
  }
  lvls = getTaskClassLevels(.task)
  # probs is always complete, if a class is empty is has 0 frequency in probs
  probs = prop.table(table(y))
  list(method = method, probs = probs)
}

#' @export
predictLearner.classif.featureless = function(.learner, .model, .newdata, ...) {
  # extract some shortcuts
  n = nrow(.newdata)
  ptype = .learner$predict.type
  mod = getLearnerModel(.model)
  labels = names(mod$probs)
  probs = as.numeric(mod$probs)
  method = mod$method

  if (ptype == "response") {
    if (method == "majority") {
      # take label with max prior value. When ties.method != "random" and there are ties,
      # we select either the first or the last class, according to the value of ties.method.
      j = rep(getMaxIndex(probs, ties.method = "random"), n)
      p = factor(labels[j], levels = labels)
    } else if (method == "sample-prior") {
      # sample with prior probability from observed labels
      p = factor(sample(labels, size = n, prob = probs, replace = TRUE), levels = labels)
    }
  } else if (ptype == "prob") {
    p = matrix(probs, nrow = n, ncol = length(probs), byrow = TRUE)
    colnames(p) = labels
  }
  return(p)
}
