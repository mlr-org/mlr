
#FIXME: we should have the learner produce its own labels.
# this  is a generat thing for all learners!
# FIXME: check how doc is done for random forest


#' @title Dummy classification learner.
#'
#' @description
#' Very basic baseline method, mainly useful in model comparisons (if you don't beat this
#' you very likely have a problem).
#' Does not consider features in task, only looks at labels oin training data.
#'
#' Method \dQuote{majority} predicts always the mojority class for each new observation. If
#'
#' Method \dQuote{sample-prior} samples a random label for each new observation, where each label
#' probability coincied with the proportion of that label obsered in the training data.
#'
#' If you opt to predict probabilities, the label is always computed by mlr based on the predicted probabilities, see here:
#'
#' @export
makeRLearner.classif.dummy = function() {
  makeRLearnerClassif(
    cl = "classif.dummy",
    package = "mlr",
    par.set = makeParamSet(
      makeDiscreteLearnerParam(id = "method", default = "majority", values = c("majority", "sample-prior"))
    ),
    # FIXME: check properties
    properties = c("twoclass", "multiclass", "numerics", "factors", "prob"),
    name = "Featureless dummy predictor",
    short.name = "dummy",
    # FIXME: doc on page
    note = "Simple baseline meathod. Either always predicts the majority class in the training data or sampkles "
  )
}

#' @export
trainLearner.classif.dummy = function(.learner, .task, .subset, .weights = NULL,  ...) {
  y = getTaskTargets(.task)[.subset]
  # FIXME: use weights
  # # FIXME: we need to ensure all classes are here
  probs = prop.table(table(y))
  list(probs = probs)
}

#' @export
predictLearner.classif.dummy = function(.learner, .model, .newdata, ...) {
  # extract some shortcuts
  n = nrow(.newdata)
  ptype = .learner$predict.type
  labels = names(.model$learner.model$probs)
  probs = as.numeric(.model$learner.model$probs)
  method = .learner$par.vals$method

  if (ptype == "response") {
    if (method == "majority") {
      # FIXME: sample in case of ties for EACH obs
      # just take label with max prior value. in case of ties we select randomly
      j = getMaxIndex(probs, ties.method = "random")
      p = factor(rep(labels[j], n))
    } else if (method == "sample-prior") {
      # sample with prior probability from observed labels
      p = factor(sample(labels, size = n, prob = probs, replace = TRUE))
    }
  } else if (ptype == "prob") {
    p = matrix(probs, nrow = n, ncol = length(probs), byrow = TRUE)
    colnames(p) = labels
  }
  return(p)
}

