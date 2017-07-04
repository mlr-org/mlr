#' @title Fuse learner with the bagging technique.
#'
#' @description
#' Fuses a learner with the bagging method
#' (i.e., similar to what a \code{randomForest} does).
#' Creates a learner object, which can be
#' used like any other learner object.
#' Models can easily be accessed via \code{\link{getLearnerModel}}.
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
#'   At least one feature will always be selected.
#' @template ret_learner
#' @family wrapper
#' @export
makeAMVhdWrapper = function(learner, amv.iters = 10L, amv.feats = 3) {
  learner = checkLearner(learner, type = c("oneclass"))
  pv = list()
  if (!missing(amv.iters)) {
    amv.iters = asInt(amv.iters, lower = 1L)
    pv$amv.iters = amv.iters
  }
  if (!missing(amv.feats)) {
    assertNumber(amv.feats, lower = 0, upper = 1)
    pv$amv.feats = amv.feats
  }
  if (learner$predict.type != "prob")
    stop("Predict type for AMVhd learner must be 'prob'.")
  id = stri_paste(learner$id, "AMVhd", sep = ".")
  packs = learner$package
  ps = makeParamSet(
    makeIntegerLearnerParam(id = "amv.iters", lower = 1L, default = 10L),
    makeNumericLearnerParam(id = "amv.feats", lower = 2, upper = 7, default = 3)
  )
  makeHomogeneousEnsemble(id, learner$type, learner, packs, par.set = ps, par.vals = pv,
    learner.subclass = "AMVhdWrapper", model.subclass = "AMVhdModel")
}

#' @export
print.AMVhdModel = function(x, ...) {
  s = capture.output(print.WrappedModel(x))
  u = sprintf("AMVhd Learner: %s", class(x$learner$next.learner)[1L])
  s = append(s, u, 1L)
  lapply(s, catf)
}

#' @export
trainLearner.AMVhdWrapper = function(.learner, .task, .subset = NULL, .weights = NULL,
  amv.iters = 10, amv.feats = 3, ...) {
  .task = subsetTask(.task, subset = .subset)
  d = getTaskNFeats(.task)
  # number of observations to sample
  ###m = round(n * bw.size)
  # number of features to sample
  ###k = max(round(bw.feats * getTaskNFeats(.task)), 1)
  fullmodel = train(.learner$next.learner, .task)
  args = list(d = d, dsub = amv.feats,  task = .task, learner = .learner, weights = .weights)
  parallelLibrary("mlr", master = FALSE, show.info = FALSE)
  #??exportMlrOptions(level = "mlr.ensemble")
  models = parallelMap(doAMVTrainIteration, i = seq_len(amv.iters), more.args = args)
  models[[amv.iters+1]] =  fullmodel
  models = rev(models)
  makeHomChainModel(.learner, models)
}

doAMVTrainIteration = function(i, d, dsub, task, learner, weights) {
  setSlaveOptions()
  task = subsetTask(task, features = sample(getTaskFeatureNames(task), dsub, replace = FALSE))
  train(learner$next.learner, task, weights = weights)
}

#' @export
predictLearner.AMVhdWrapper = function(.learner, .model, .newdata, .subset = NULL, ...) {
  models = getLearnerModel(.model, more.unwrap = FALSE)
  if(.learner$predict.type != "prob")
    stop("Predict type for AMVhd learner must be 'prob'.")
  #g = if (.learner$type == "classif") as.character else identity
  pred = lapply(models, function(m) {
    nd = .newdata[, m$features, drop = FALSE]
    p.tmp = predict(m, newdata = nd, subset = .subset, ...)$data[ ,1:2] #take prob column
    #p.tmp = predict(models[[1]], newdata = .newdata[, models[[1]]$features, drop = FALSE], subset = .subset)$data[ ,1:2]
    colnames(p.tmp) = .model$task.desc$class.levels
    as.matrix(p.tmp)
  })
  p = pred[[1]]
  attr(p, "pred.sub") = pred[-1]
  addClasses(p, "AMVhdpredict")
}


#' @export
print.AMVhdpredict = function(x, ...) {
  catf("Prediction: %i observations", nrow(x$data))
  catf("predict.type: %s", x$predict.type)
  catf("threshold: %s", collapse(sprintf("%s=%.2f", names(x$threshold), x$threshold)))
  catf("time: %.2f", x$time)
  if (!is.na(x$error)) catf("errors: %s", x$error)
  printHead(as.data.frame(x), ...)
}
# we need to override here. while the predtype of the encapsulated learner must always
# be response, we can estimates probs and se on the outside
#' @export
setPredictType.AMVhdWrapper = function(learner, predict.type) {
  setPredictType.Learner(learner, predict.type)
}

#' @export
getLearnerProperties.AMVhdWrapper = function(learner) {
    switch(learner$type,
    "oneclass" = union(getLearnerProperties(learner$next.learner), "prob")
  )
}
