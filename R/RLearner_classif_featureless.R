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
#' observation. In case of ties, the predictions will depend on the selected
#' \dQuote{ties.method}, i.e., either always the first factor level, always the
#' last factor level or a randomly sampled class for each observation is predicted.
#'
#' Method \dQuote{sample-prior} always samples a random class for each new
#' observation according to the prior probabilities observed in the training data.
#'
#' The default method is \dQuote{majority} with \dQuote{ties.method = "first"}
#' which corresponds to the ZeroR classifier from WEKA, see
#' \url{https://weka.wikispaces.com/ZeroR}.
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
      makeDiscreteLearnerParam(id = "method", default = "majority", values = c("majority", "sample-prior")),
      makeDiscreteLearnerParam(id = "ties.method", default = "first",
        values = c("random", "first", "last"), requires = quote(method=="majority"))
    ),
    properties = c("twoclass", "multiclass", "numerics", "factors", "ordered", "missings", "prob"),
    name = "Featureless classifier",
    short.name = "featureless",
    note = "The default method is `majority` and corresponds to the ZeroR classifier from WEKA."
  )
}

#' @export
trainLearner.classif.featureless = function(.learner, .task, .subset, .weights = NULL,
  method = "majority", ties.method = "first", ...) {
  y = getTaskTargets(.task)[.subset]
  lvls = getTaskClassLevels(.task)
  probs = prop.table(table(y))
  list(method = method, ties.method = ties.method, probs = probs)
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
      # ZeroR uses always the first factor level, see http://weka.8497.n7.nabble.com/ZeroR-on-dataset-with-equal-modes-td33355.html
      ties.method = mod$ties.method
      if (any(duplicated(probs)) & ties.method == "random") {
        # if there are ties and ties.method = "random", sample for EACH obs (more expensive)
        p.mat = matrix(probs, nrow = n, ncol = length(probs), byrow = TRUE)
        j = getMaxIndexOfRows(p.mat, ties.method = ties.method)
      } else {
        # take label with max prior value. When ties.method != "random" and there are ties,
        # we select either the first or the last class, according to the value of ties.method.
        j = rep(getMaxIndex(probs, ties.method = ties.method), n)
      }
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
