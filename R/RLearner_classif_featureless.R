#FIXME: we should have the learner produce its own labels.
# this  is a generat thing for all learners!
# FIXME: check how doc is done for random forest

#' @title Featureless classification learner.
#'
#' @description
#' Very basic baseline method, mainly useful in model comparisons (if you don't
#' beat this you very likely have a problem).
#' Does not consider features of the task and only looks at labels on training
#' data.
#'
#' Method \dQuote{majority} predicts always the majority class for each new
#' observation. In case of ties, we pick a randomly sampled class and always
#' predict that clas for each new observation.
#'
#' Method \dQuote{sample-prior} samples a random label for each new observation,
#' where each label probability coincied with the proportion of that label
#' obsered in the training data.
#'
#' If you opt to predict probabilities, the label is always computed by mlr
#' based on the prior probabilities obsered in the training data.
#'
#' @export
makeRLearner.classif.featureless = function() {
  makeRLearnerClassif(
    cl = "classif.featureless",
    package = "mlr",
    par.set = makeParamSet(
      makeDiscreteLearnerParam(id = "method", default = "majority", values = c("majority", "sample-prior"))
    ),
    properties = c("twoclass", "multiclass", "numerics", "factors", "ordered", "missings", "prob"),
    name = "Featureless classifier",
    short.name = "featureless",
    note = "Simple baseline method. Either always predicts the majority class or samples the predicted class according to the prior probabilities in the training data."
  )
}

#' @export
trainLearner.classif.featureless = function(.learner, .task, .subset, .weights = NULL, ...) {
  y = getTaskTargets(.task)[.subset]
  lvls = getTaskClassLevels(.task)

  # FIXME: use weights
  probs = prop.table(table(y))

  # we need to ensure all classes are here (GC: this is maybe not necessary, but it was a FIXME and doing this does not hurt)
  missing.lvl = setdiff(lvls, names(probs))
  if (length(missing.lvl) != 0) {
    ll = c(levels(y), missing.lvl)
    y = factor(y, levels = ll)
    probs = prop.table(table(y))
  }

  list(probs = probs)
}

#' @export
predictLearner.classif.featureless = function(.learner, .model, .newdata, ...) {
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
