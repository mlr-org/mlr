#' @title Use stacking method (stacked generalization) to create a multilabel learner.
#'
#' @description
#' Every learner which is implemented in mlr and which supports binary
#' classification can be converted to a wrapped stacking multilabel learner.
#' Stacking trains a binary classifier for each label using predicted label information of all labels (including the target label)
#' as additional features (by cross validation).
#' During prediction these labels need are obtained by the binary relevance method using the same binary learner.
#'
#' Models can easily be accessed via [getLearnerModel].
#'
#' @template arg_learner
#' @template arg_multilabel_cvfolds
#' @template ret_learner
#' @references
#' Montanes, E. et al. (2013)
#' *Dependent binary relevance models for multi-label classification*
#' Artificial Intelligence Center, University of Oviedo at Gijon, Spain.
#' @family wrapper
#' @family multilabel
#' @export
#' @example inst/examples/MultilabelWrapper.R
makeMultilabelStackingWrapper = function(learner, cv.folds = 2) {
  learner = checkLearner(learner, type = "classif", props = "twoclass")
  id = stri_paste("multilabel.stacking", getLearnerId(learner), sep = ".")
  packs = getLearnerPackages(learner)
  type = getLearnerType(learner)
  x = makeHomogeneousEnsemble(id, type, learner, packs, learner.subclass = "MultilabelStackingWrapper", model.subclass = "MultilabelStackingModel")
  x$type = "multilabel"
  x$cv.folds = cv.folds
  return(x)
}

#' @export
trainLearner.MultilabelStackingWrapper = function(.learner, .task, .subset = NULL, .weights = NULL, ...) {

  targets = getTaskTargetNames(.task)
  .task = subsetTask(.task, subset = .subset)
  data = getTaskData(.task)
  # train level 1 learners
  models.lvl1 = getLearnerModel(train(makeMultilabelBinaryRelevanceWrapper(.learner$next.learner), .task, weights = .weights))
  # predict labels
  f = function(tn) {
    data2 = dropNamed(data, setdiff(targets, tn))
    ctask = makeClassifTask(id = tn, data = data2, target = tn)
    rdesc = makeResampleDesc("CV", iters = .learner$cv.folds)
    r = resample(.learner$next.learner, ctask, rdesc, weights = .weights, show.info = FALSE)
    as.numeric(as.logical(r$pred$data[order(r$pred$data$id), ]$response)) # did not use getPredictionResponse, because of ordering
  }
  pred.labels = sapply(targets, f)
  # train meta level learners
  g = function(tn) {
    data.meta = dropNamed(data.frame(data, pred.labels), setdiff(targets, tn))
    ctask = makeClassifTask(id = tn, data = data.meta, target = tn)
    train(.learner$next.learner, ctask, weights = .weights)
  }
  models.meta = lapply(targets, g)
  makeHomChainModel(.learner, c(models.lvl1, models.meta))
}

#' @export
predictLearner.MultilabelStackingWrapper = function(.learner, .model, .newdata, .subset = NULL, ...) {

  models = getLearnerModel(.model, more.unwrap = FALSE)
  # Level 1 prediction (binary relevance)
  models.lvl1 = models[seq_along(.model$task.desc$target)]
  f = if (.learner$predict.type == "response") {
    function(m) as.logical(getPredictionResponse(predict(m, newdata = .newdata, subset = .subset, ...)))
  } else {
    function(m) getPredictionProbabilities(predict(m, newdata = .newdata, subset = .subset, ...), cl = "TRUE")
  }
  if (.learner$predict.type == "response") {
    pred.lvl1 = sapply(data.frame(asMatrixCols(lapply(models.lvl1, f))), as.numeric)
  } else {
    pred.lvl1 = data.frame(asMatrixCols(lapply(models.lvl1, f)))
  }
  colnames(pred.lvl1) = paste(.model$task.desc$target, ".1", sep = "")
  # Meta level prediction
  models.meta = models[(length(.model$task.desc$target) + 1):(2 * length(.model$task.desc$target))]
  nd = data.frame(.newdata, pred.lvl1)
  g = if (.learner$predict.type == "response") {
    function(m) as.logical(getPredictionResponse(predict(m, newdata = nd, subset = .subset, ...)))
  } else {
    function(m) getPredictionProbabilities(predict(m, newdata = nd, subset = .subset, ...), cl = "TRUE")
  }
  asMatrixCols(lapply(models.meta, g), col.names = .model$task.desc$target)
}
