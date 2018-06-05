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

  x = list(regr = x.regr, classif = x.classif)
  x = addClasses(x, c("Learner", "MixedOutputSimpleWrapperOneForAll"))
  x$fix.factors.prediction = FALSE
  x$package = "stats"
  x$type = "mixedoutput"

  return(x)
}

#' @export
trainLearner.MixedOutputSimpleWrapperOneForAll = function(.learner, .task, .subset = NULL, .weights = NULL, ...) {
  .task = subsetTask(.task, subset = .subset)

  tt = .task$target.type
  classif.targets = names(tt)[tt == "factor"]
  regr.targets = names(tt)[tt == "numeric"]

  #train classif
  parallelLibrary("mlr", master = FALSE, level = "mlr.ensemble", show.info = FALSE)
  exportMlrOptions(level = "mlr.ensemble")
  models.classif = parallelMap(
    doMixedOutputSimpleClassifTrainIteration, tn = classif.targets,
    more.args = list(weights = .weights, learner = .learner$classif$next.learner, task = .task),
    level = "mlr.ensemble")
  names(models.classif) = classif.targets

  #train regr
  parallelLibrary("mlr", master = FALSE, level = "mlr.ensemble", show.info = FALSE)
  exportMlrOptions(level = "mlr.ensemble")
  models.regr = parallelMap(
    doMixedOutputSimpleRegrTrainIteration, tn = regr.targets,
    more.args = list(weights = .weights, learner = .learner$regr$next.learner, task = .task),
    level = "mlr.ensemble")
  names(models.regr) = regr.targets


  list(regr = makeHomChainModel(.learner$regr, models.regr),
    classif = makeHomChainModel(.learner$classif, models.classif)
  )
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
trainLearner.MultiOutputWrapper = function(.learner, .task, .subset = NULL, .weights = NULL, ...) {
  # targets = getTaskTargetNames(.task)
  # .task = subsetTask(.task, subset = .subset)
  # data = getTaskData(.task)
  # # train level 1 learners
  # models.lvl1 = getLearnerModel(train(makeMultilabelBinaryRelevanceWrapper(.learner$next.learner), .task, weights = .weights))
  # # predict labels
  # f = function(tn) {
  #   data2 = dropNamed(data, setdiff(targets, tn))
  #   ctask = makeClassifTask(id = tn, data = data2, target = tn)
  #   rdesc = makeResampleDesc("CV", iters = .learner$cv.folds)
  #   r = resample(.learner$next.learner, ctask, rdesc, weights = .weights, show.info = FALSE)
  #   as.numeric(as.logical(r$pred$data[order(r$pred$data$id), ]$response)) #did not use getPredictionResponse, because of ordering
  # }
  # pred.labels = sapply(targets, f)
  # # train meta level learners
  # g = function(tn) {
  #   data.meta = dropNamed(data.frame(data, pred.labels), setdiff(targets, tn))
  #   ctask = makeClassifTask(id = tn, data = data.meta, target = tn)
  #   train(.learner$next.learner, ctask, weights = .weights)
  # }
  # models.meta = lapply(targets, g)
  # makeHomChainModel(.learner, c(models.lvl1, models.meta))
}

#' @export
predictLearner.MultilabelStackingWrapper = function(.learner, .model, .newdata, .subset = NULL, ...) {
  # models = getLearnerModel(.model, more.unwrap = FALSE)
  # # Level 1 prediction (binary relevance)
  # models.lvl1 = models[seq_along(.model$task.desc$target)]
  # f = if (.learner$predict.type == "response") {
  #   function(m) as.logical(getPredictionResponse(predict(m, newdata = .newdata, subset = .subset, ...)))
  # } else {
  #   function(m) getPredictionProbabilities(predict(m, newdata = .newdata, subset = .subset, ...), cl = "TRUE")
  # }
  # if (.learner$predict.type == "response") {
  #   pred.lvl1 = sapply(data.frame(asMatrixCols(lapply(models.lvl1, f))), as.numeric)
  # } else {
  #   pred.lvl1 = data.frame(asMatrixCols(lapply(models.lvl1, f)))
  # }
  # colnames(pred.lvl1) = paste(.model$task.desc$target, ".1", sep = "")
  # # Meta level prediction
  # models.meta = models[(length(.model$task.desc$target) + 1):(2 * length(.model$task.desc$target))]
  # nd = data.frame(.newdata, pred.lvl1)
  # g = if (.learner$predict.type == "response") {
  #   function(m) as.logical(getPredictionResponse(predict(m, newdata = nd, subset = .subset, ...)))
  # } else {
  #   function(m) getPredictionProbabilities(predict(m, newdata = nd, subset = .subset, ...), cl = "TRUE")
  # }
  # asMatrixCols(lapply(models.meta, g), col.names = .model$task.desc$target)
}
