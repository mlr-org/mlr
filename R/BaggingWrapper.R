#' @title Fuse learner with the bagging technique.
#'
#' @description
#' Fuses a learner with the bagging method
#' (i.e., similar to what a `randomForest` does).
#' Creates a learner object, which can be
#' used like any other learner object.
#' Models can easily be accessed via [getLearnerModel].
#'
#' Bagging is implemented as follows:
#' For each iteration a random data subset is sampled (with or without replacement)
#' and potentially the number of features is also restricted to
#' a random subset. Note that this is usually handled in a slightly different way
#' in the random forest where features are sampled at each tree split).
#'
#' Prediction works as follows:
#' For classification we do majority voting to create a discrete label and
#' probabilities are predicted by considering the proportions of all predicted labels.
#' For regression the mean value and the standard deviations across predictions is computed.
#'
#' Note that the passed base learner must always have `predict.type = 'response'`,
#' while the BaggingWrapper can estimate probabilities and standard errors, so it can
#' be set, e.g., to `predict.type = 'prob'`. For this reason, when you call
#' [setPredictType], the type is only set for the BaggingWrapper, not passed
#' down to the inner learner.
#'
#' @template arg_learner
#' @param bw.iters (`integer(1)`)\cr
#'   Iterations = number of fitted models in bagging.
#'   Default is 10.
#' @param bw.replace (`logical(1)`)\cr
#'   Sample bags with replacement (bootstrapping)?
#'   Default is TRUE.
#' @param bw.size (`numeric(1)`)\cr
#'   Percentage size of sampled bags.
#'   Default is 1 for bootstrapping and 0.632 for subsampling.
#' @param bw.feats (`numeric(1)`)\cr
#'   Percentage size of randomly selected features in bags.
#'   Default is 1.
#'   At least one feature will always be selected.
#' @template ret_learner
#' @family wrapper
#' @export
makeBaggingWrapper = function(learner, bw.iters = 10L, bw.replace = TRUE, bw.size, bw.feats = 1) {

  learner = checkLearner(learner, type = c("classif", "regr"))
  pv = list()
  if (!missing(bw.iters)) {
    bw.iters = asInt(bw.iters, lower = 1L)
    pv$bw.iters = bw.iters
  }
  if (!missing(bw.replace)) {
    assertFlag(bw.replace)
    pv$bw.replace = bw.replace
  }
  if (!missing(bw.size)) {
    assertNumber(bw.size, lower = 0, upper = 1)
    pv$bw.size = bw.size
  }
  if (!missing(bw.feats)) {
    assertNumber(bw.feats, lower = 0, upper = 1)
    pv$bw.feats = bw.feats
  }
  if (learner$predict.type != "response") {
    stop("Predict type of the basic learner must be 'response'.")
  }
  id = stri_paste(learner$id, "bagged", sep = ".")
  packs = learner$package
  ps = makeParamSet(
    makeIntegerLearnerParam(id = "bw.iters", lower = 1L, default = 10L),
    makeLogicalLearnerParam(id = "bw.replace", default = TRUE),
    makeNumericLearnerParam(id = "bw.size", lower = 0, upper = 1),
    makeNumericLearnerParam(id = "bw.feats", lower = 0, upper = 1, default = 2 / 3)
  )
  makeHomogeneousEnsemble(id, learner$type, learner, packs, par.set = ps, par.vals = pv,
    learner.subclass = "BaggingWrapper", model.subclass = "BaggingModel")
}

#' @export
print.BaggingModel = function(x, ...) {
  s = capture.output(print.WrappedModel(x))
  u = sprintf("Bagged Learner: %s", class(x$learner$next.learner)[1L])
  s = append(s, u, 1L)
  lapply(s, catf)
}

#' @export
trainLearner.BaggingWrapper = function(.learner, .task, .subset = NULL, .weights = NULL,
  bw.iters = 10, bw.replace = TRUE, bw.size, bw.feats = 1, ...) {

  if (missing(bw.size)) {
    bw.size = if (bw.replace) 1 else 0.632
  }
  .task = subsetTask(.task, subset = .subset)
  n = getTaskSize(.task)
  # number of observations to sample
  m = round(n * bw.size)
  # number of features to sample
  k = max(round(bw.feats * getTaskNFeats(.task)), 1)

  args = list(n = n, m = m, k = k, bw.replace = bw.replace,
    task = .task, learner = .learner, weights = .weights)
  parallelLibrary("mlr", master = FALSE, level = "mlr.ensemble", show.info = FALSE)
  exportMlrOptions(level = "mlr.ensemble")
  models = parallelMap(doBaggingTrainIteration, i = seq_len(bw.iters), more.args = args, level = "mlr.ensemble")
  makeHomChainModel(.learner, models)
}

doBaggingTrainIteration = function(i, n, m, k, bw.replace, task, learner, weights) {
  setSlaveOptions()
  bag = sample(seq_len(n), m, replace = bw.replace)
  task = subsetTask(task, features = sample(getTaskFeatureNames(task), k, replace = FALSE))
  train(learner$next.learner, task, subset = bag, weights = weights[bag])
}

#' @export
predictLearner.BaggingWrapper = function(.learner, .model, .newdata, .subset = NULL, ...) {
  models = getLearnerModel(.model, more.unwrap = FALSE)
  g = if (.learner$type == "classif") as.character else identity
  p = asMatrixCols(lapply(models, function(m) {
    nd = .newdata[, m$features, drop = FALSE]
    g(predict(m, newdata = nd, subset = .subset, ...)$data$response)
  }))
  if (.learner$predict.type == "response") {
    if (.learner$type == "classif") {
      as.factor(apply(p, 1L, computeMode))
    } else {
      rowMeans(p)
    }
  } else {
    if (.learner$type == "classif") {
      levs = .model$task.desc$class.levels
      p = apply(p, 1L, function(x) {
        x = factor(x, levels = levs) # we need all level for the table and we need them in consistent order!
        as.numeric(prop.table(table(x)))
      })
      setColNames(t(p), levs)
    } else {
      cbind(rowMeans(p), apply(p, 1L, sd))
    }
  }
}

# we need to override here. while the predtype of the encapsulated learner must always
# be response, we can estimates probs and se on the outside
#' @export
setPredictType.BaggingWrapper = function(learner, predict.type) {
  setPredictType.Learner(learner, predict.type)
}

#' @export
getLearnerProperties.BaggingWrapper = function(learner) {
  switch(learner$type,
    "classif" = union(getLearnerProperties(learner$next.learner), "prob"),
    "regr" = union(getLearnerProperties(learner$next.learner), "se")
  )
}
