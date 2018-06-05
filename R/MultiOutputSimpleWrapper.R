#' @title ADD TITLE #FIXME
#'
#' @description
#' ADD DESCRIPTION #FIXME
#'
#' Models can easily be accessed via [getLearnerModel].
#'
#' @template arg_learner
#' @template arg_multilabel_cvfolds
#' @template ret_learner
#' @references
#' ADD REFRENCES #FIXME
#' @family wrapper
#' @family multilabel
#' @family multiregr
#' @family mixedoutput
#' @export
#' @example # add example FIXME#inst/examples/MultilabelWrapper.R
makeMultiOutputSimpleWrapper = function(output.type = "mixedoutput", regr, classif, individual = FALSE) {
  if (!output.type %in% c("mixedoutput", "multilabel", "multiregr")) stop("type must be 'mixedoutput', 'multilabel', or 'multiregr'")
  regr = checkLearner(regr)
  classif = checkLearner(classif)

  #regr
  id = stri_paste(output.type, "simplewrapper", getLearnerId(regr), sep = ".")
  packs = getLearnerPackages(regr)
  type = getLearnerType(regr)
  x.regr = makeHomogeneousEnsemble(id, type, regr, packs, learner.subclass = "MultioutputSimpleWrapper",
    model.subclass = "MultioutputModel")

  #classif
  id = stri_paste(output.type, "simplewrapper", getLearnerId(classif), sep = ".")
  packs = getLearnerPackages(classif)
  type = getLearnerType(classif)
  x.classif = makeHomogeneousEnsemble(id, type, classif, packs, learner.subclass = "MultioutputSimpleWrapper",
    model.subclass = "MultioutputModel")

  packs = "stats" #packages are checked above, this is needed anyhow
  type = "mixedoutput"
  id = stri_paste(output.type, "simplewrapper", sep = ".")
  learner = makeLearner("classif.featureless") #we need a dummy learner for checks...
  x = makeHomogeneousEnsemble(id, type, learner, packs,
    learner.subclass = "MultioutputSimpleWrapper", model.subclass = "MultioutputSimpleModel")
  x$type = "mixedoutput"
  x$univ.learners = list(regr = x.regr, classif = x.classif)
  return(x)
}

#' @export
trainLearner.MultioutputSimpleWrapper = function(.learner, .task, .subset = NULL, .weights = NULL, ...) {
  .task = subsetTask(.task, subset = .subset)

  tt = .task$target.type
  classif.targets = names(tt)[tt == "factor"]
  regr.targets = names(tt)[tt == "numeric"]

  #train classif
  parallelLibrary("mlr", master = FALSE, level = "mlr.ensemble", show.info = FALSE)
  exportMlrOptions(level = "mlr.ensemble")
  models.classif = parallelMap(
    doMixedOutputSimpleClassifTrainIteration, tn = classif.targets,
    more.args = list(weights = .weights, learner = .learner$univ.learners$classif$next.learner, task = .task),
    level = "mlr.ensemble")
  names(models.classif) = classif.targets

  #train regr
  parallelLibrary("mlr", master = FALSE, level = "mlr.ensemble", show.info = FALSE)
  exportMlrOptions(level = "mlr.ensemble")
  models.regr = parallelMap(
    doMixedOutputSimpleRegrTrainIteration, tn = regr.targets,
    more.args = list(weights = .weights, learner = .learner$univ.learners$regr$next.learner, task = .task),
    level = "mlr.ensemble")
  names(models.regr) = regr.targets

  makeHomChainModel(.learner, append(models.regr, models.classif))
}

doMixedOutputSimpleClassifTrainIteration = function(tn, learner, task, weights) {
  setSlaveOptions()
  data = getTaskData(task)
  task = makeClassifTask(id = tn, data = dropNamed(data, setdiff(getTaskTargetNames(task), tn)), target = tn)
  train(learner, task, weights = weights)
}

doMixedOutputSimpleRegrTrainIteration = function(tn, learner, task, weights) {
  setSlaveOptions()
  data = getTaskData(task)
  task = makeRegrTask(id = tn, data = dropNamed(data, setdiff(getTaskTargetNames(task), tn)), target = tn)
  train(learner, task, weights = weights)
}

#' @export
predictLearner.MultioutputSimpleWrapper = function(.learner, .model, .newdata, .subset = NULL, ...) {
  models = getLearnerModel(.model, more.unwrap = FALSE)
  f = function(m) getPredictionResponse(predict(m, newdata = .newdata, subset = .subset, ...))
  data.frame(lapply(models, f))
}
