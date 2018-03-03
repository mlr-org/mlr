#' @title Preprocessing for using the anomaly detecion performance measure
#' Area under the Mass Volume Curve for high dimensional data (AMVhd).
#'
#' @description
#' For prediction on data with dimension higher than eight the AMV should not be
#' used directly as performance measure (see \code{makeAMVMeasure}).
#' The basic idea is that the wrapper does several feature sub-samplings
#' (of dimension less than 8) to reduce the dimension of the subsets and applying
#' the model and the prediction on each subsample.
#' Afterwards AMV can be applied on each subsamples, yielding partial scores AMV_k.
#' The mean of the partial scores is the new performance criteria AMVhd (see \code{makeAMVhdMeasure}).
#'
#' The wrapper is used within the AMVhd measure.
#'
#' Training is implemented as follows:
#' For each iteration a data subset with random feature subsample is drawn
#' (without replacement). On each subset the model is trained with the base learner.
#' Addtionally, the model on the full data set is also trained. The training with
#' the wrapper returns a list of all models. The first element of the list contrains
#' the model on the full data set.
#'
#' Prediction works as follows:
#' For every model from the training with the wrapper the prediction is calculated
#' on the test set with the corresponding feature sample. The prediction object
#' returns the prediction of the model, which is trained on the full data set and
#' additionally returns an attribute 'AMVhdSubpredict' which contains the prediction
#' of all subsamples. In \code{makeAMVhdMeasure} AMV is calculated for each subsample
#' and aggregated to AMVhd.
#'
#' Note that the passed base learner must always have \code{predict.type = 'prob'}.
#'
#' @template arg_learner
#' @param amv.iters [\code{integer(1)}]\cr
#'   Iterations = number of fitted sub models.
#'   Default is 10.
#' @param amv.feats [\code{numeric(1)}]\cr
#'   Size of randomly selected features for the sub models.
#'   Default is 3.
#' @references Nicolas, G. How to Evaluate the Quality of Unsupervised Anomaly Detection Algorithms,
#' arXiv preprint arXiv:1607.01152
#' @note see example of \code{makeAMVhdMeasure}
#' @template ret_learner
#' @family wrapper
#' @export

makeAMVhdWrapper = function(learner, amv.iters, amv.feats) {
  learner = checkLearner(learner, type = "oneclass")
  pv = list()
  if (!missing(amv.iters)) {
    amv.iters = asInt(amv.iters, lower = 1L)
    pv$amv.iters = amv.iters
  }
  if (!missing(amv.feats)) {
    assertNumber(amv.feats, lower = 0L, upper = 7L)
    pv$amv.feats = amv.feats
  }
  if (learner$predict.type != "prob")
    stop("Predict type for AMVhd learner must be 'prob'.")
  id = stri_paste(learner$id, "AMVhd", sep = ".")
  packs = learner$package
  ps = makeParamSet(
    makeIntegerLearnerParam(id = "amv.iters", lower = 1L, default = 10L),
    makeNumericLearnerParam(id = "amv.feats", lower = 2L, upper = 7L, default = 3L)
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
  amv.iters, amv.feats, ...) {
  .task = subsetTask(.task, subset = .subset)
  d = getTaskNFeats(.task)

  fullmodel = train(.learner$next.learner, .task)
  args = list(d = d, dsub = amv.feats,  task = .task, learner = .learner, weights = .weights)
  parallelLibrary("mlr", master = FALSE, show.info = FALSE)
  #exportMlrOptions(level = "mlr.ensemble")
  models = parallelMap(doAMVhdTrainIteration, i = seq_len(amv.iters), more.args = args)
  models[[amv.iters + 1]] =  fullmodel
  models = rev(models)
  makeHomChainModel(.learner, models)
}

doAMVhdTrainIteration = function(i, d, dsub, task, learner, weights) {
  setSlaveOptions()
  task = subsetTask(task, features = sample(getTaskFeatureNames(task), dsub, replace = FALSE))
  train(learner$next.learner, task, weights = weights)
}

#' @export
predictLearner.AMVhdWrapper = function(.learner, .model, .newdata, .subset = NULL, ...) {
  models = getLearnerModel(.model, more.unwrap = FALSE)
  if (.learner$predict.type != "prob")
    stop("Predict type for AMVhd learner must be 'prob'.")
  pred = lapply(models, function(m) {
    nd = .newdata[, m$features, drop = FALSE]
    p.tmp = predict(m, newdata = nd, subset = .subset, ...)$data[, 1:2] # take prob column
    colnames(p.tmp) = .model$task.desc$class.levels
    p.tmp = as.matrix(p.tmp)
    attr(p.tmp, "n.subfeat") = length(m$features)
    attr(p.tmp, "subfeat") = m$features
    p.tmp
  })
  addClasses(pred, "PredictionAMVhd")
}

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
