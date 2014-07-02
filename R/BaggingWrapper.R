#' @title Fuse learner with the bagging technique.
#'
#' @description
#' Fuses a learner with the bagging method
#' (i.e., similar to what a \code{randomForest} does).
#' Creates a learner object, which can be
#' used like any other learner object.
#' Models can easily be accessed via \code{\link{getBaggingModels}}.
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
#' Note that the passed base learner must always have \code{predict.type = 'response'},
#' while the BaggingWrapper can estimate probabilities and standard errors, so it can
#' be set, e.g., to \code{predict.type = 'prob'}. For this reason, when you call
#' \code{\link{setPredictType}}, the type is only set for the BaggingWrapper, not passed
#' down to the inner learner.
#'
#' @template arg_learner
#' @param bw.iters [\code{integer(1)}]\cr
#'   Iterations = number of fitted models in bagging.
#'   Default is 10.
#' @param bw.replace [\code{logical(1)}]\cr
#'   Sample bags with replacement (bootstrapping)?
#'   Default is TRUE.
#' @param bw.size [\code{numeric(1)}]\cr
#'   Percentage size of sampled bags.
#'   Default is 1 for bootstrapping and 0.632 for subsampling.
#' @param bw.feats [\code{numeric(1)}]\cr
#'   Percentage size of randomly selected features in bags.
#'   Default is 1.
#' @template ret_learner
#' @export
makeBaggingWrapper = function(learner, bw.iters = 10L, bw.replace = TRUE, bw.size, bw.feats = 1) {

  learner = checkLearner(learner)
  bw.iters = asInt(bw.iters, lower = 1L)
  assertFlag(bw.replace)
  if (missing(bw.size)) {
    bw.size = if (bw.replace) 1 else 0.632
  } else {
    assertNumber(bw.size, lower = 0, upper = 1)
  }
  assertNumber(bw.feats, lower = 0, upper = 1)
  if (learner$predict.type != "response")
    stop("Predict type of the basic learner must be 'response'.")
  id = paste(learner$id, "bagged", sep = ".")
  packs = learner$package
  ps = makeParamSet(
    makeIntegerLearnerParam(id = "bw.iters", lower = 1L, default = 10L),
    makeLogicalLearnerParam(id = "bw.replace", default = TRUE),
    makeNumericLearnerParam(id = "bw.size", lower = 0, upper = 1),
    makeNumericLearnerParam(id = "bw.feats", lower = 0, upper = 1, default = 2/3)
  )
  pv = list(bw.iters = bw.iters, bw.replace = bw.replace,
    bw.size = bw.size, bw.feats = bw.feats)
  x = makeBaseWrapper(id, learner, packs, par.set = ps, par.vals = pv, cl = "BaggingWrapper")
  x = switch(x$type,
    "classif" = addProperties(x, "prob"),
    "regr" = addProperties(x, "se"))
  return(x)
}

#' @export
trainLearner.BaggingWrapper = function(.learner, .task, .subset, .weights = NULL, bw.iters, bw.replace,
  bw.size, bw.feats, ...) {

  .task = subsetTask(.task, subset = .subset)
  n = .task$task.desc$size
  m = round(n * bw.size)
  allinds = seq_len(n)
  if (bw.feats < 1) {
    feats = getTaskFeatureNames(.task)
    k = round(bw.feats * length(feats))
  }
  models = lapply(seq_len(bw.iters), function(i) {
    bag = sample(allinds, m, replace = bw.replace)
    w = .weights[bag]
    if (bw.feats < 1) {
      feats2 = sample(feats, k, replace = FALSE)
      .task2 = subsetTask(.task, features = feats2)
      train(.learner$next.learner, .task2, subset = bag, weights = w)
  } else {
      train(.learner$next.learner, .task, subset = bag, weights = w)
    }
  })
  makeChainModel(next.model = models, cl = "BaggingModel")
}

#' @export
predictLearner.BaggingWrapper = function(.learner, .model, .newdata, ...) {
  models = getBaggingModels(.model)
  g = if (.learner$type == "classif") as.character else identity
  p = sapply(models, function(m) {
    nd = .newdata[, m$features, drop = FALSE]
    g(predict(m, newdata = nd, ...)$data$response)
  })
  if (.learner$predict.type == "response") {
    g = if (.learner$type == "classif")
      as.factor(apply(p, 1L, computeMode))
    else
      rowMeans(p)
  } else {
    if (.learner$type == 'classif') {
      levs = .model$task.desc$class.levels
      p = apply(p, 1L, function(x) {
        x = factor(x, levels = levs) # we need all level for the table and we need them in consitent order!
        as.numeric(prop.table(table(x)))
      })
      setColNames(t(p), levs)
    } else {
      cbind(rowMeans(p), apply(p, 1L, sd))
    }
  }
}


#' @export
makeWrappedModel.BaggingWrapper = function(learner, learner.model, task.desc, subset, features, factor.levels, time) {
  x = NextMethod()
  addClasses(x, "OverBaggingModel")
}

#' @export
print.BaggingModel = function(x, ...) {
  s = capture.output(print.WrappedModel(x))
  u = sprintf("Bagged Learner: %s", class(x$learner$next.learner)[1L])
  s = append(s, u, 1L)
  lapply(s, catf)
}

# we need to override here. while the predtype of the encapsulated learner must always
# be response, we can estimates probs and se on the outside
#' @export
setPredictType.BaggingWrapper = function(learner, predict.type) {
  learner = setPredictType.Learner(learner, predict.type)
  return(learner)
}


