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
