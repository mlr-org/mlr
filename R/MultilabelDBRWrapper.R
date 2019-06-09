#' @title Use dependent binary relevance method (DBR) to create a multilabel learner.
#'
#' @description
#' Every learner which is implemented in mlr and which supports binary
#' classification can be converted to a wrapped DBR multilabel learner.
#' The multilabel classification problem is converted into simple binary classifications
#' for each label/target on which the binary learner is applied.
#' For each target, actual information of all binary labels (except the target variable) is used as additional features.
#' During prediction these labels need are obtained by the binary relevance method using the same binary learner.
#'
#' Models can easily be accessed via [getLearnerModel].
#'
#' @template arg_learner
#' @template ret_learner
#' @references
#' Montanes, E. et al. (2013)
#' *Dependent binary relevance models for multi-label classification*
#' Artificial Intelligence Center, University of Oviedo at Gijon, Spain.
#' @family wrapper
#' @family multilabel
#' @export
#' @example inst/examples/MultilabelWrapper.R
makeMultilabelDBRWrapper = function(learner) {
  learner = checkLearner(learner, type = "classif", props = "twoclass")
  id = stri_paste("multilabel.DBR", getLearnerId(learner), sep = ".")
  packs = getLearnerPackages(learner)
  type = getLearnerType(learner)
  x = makeHomogeneousEnsemble(id, type, learner, packs,
    learner.subclass = "MultilabelDBRWrapper",
    model.subclass = "MultilabelDBRModel")
  x$type = "multilabel"
  return(x)
}

#' @export
trainLearner.MultilabelDBRWrapper = function(.learner, .task, .subset = NULL, .weights = NULL, ...) {

  targets = getTaskTargetNames(.task)
  .task = subsetTask(.task, subset = .subset)
  data = getTaskData(.task)
  # train level 1 learners (binary relevance)
  models.lvl1 = getLearnerModel(train(makeMultilabelBinaryRelevanceWrapper(.learner$next.learner), .task, weights = .weights))
  # train meta level learners
  models.meta = namedList(targets)
  for (tn in targets) {
    data.to.meta = dropNamed(data, setdiff(targets, tn))
    true.label.data = sapply(data.frame(data[, setdiff(targets, tn)]), as.numeric)
    colnames(true.label.data) = setdiff(targets, tn)
    data.to.meta = data.frame(data.to.meta, true.label.data)
    ctask = makeClassifTask(id = tn, data = data.to.meta, target = tn)
    models.meta[[tn]] = train(.learner$next.learner, ctask, weights = .weights)
  }
  makeHomChainModel(.learner, c(models.lvl1, models.meta))
}

#' @export
predictLearner.MultilabelDBRWrapper = function(.learner, .model, .newdata, .subset = NULL, ...) {

  models = getLearnerModel(.model, more.unwrap = FALSE)
  # Level 1 prediction (binary relevance)
  models.lvl1 = models[seq_along(.model$task.desc$target)]
  f = if (.learner$predict.type == "response") {
    function(m) as.logical(getPredictionResponse(predict(m, newdata = .newdata, subset = .subset, ...)))
  } else {
    function(m) getPredictionProbabilities(predict(m, newdata = .newdata, subset = .subset, ...), cl = "TRUE")
  }
  pred.lvl1 = sapply(data.frame(asMatrixCols(lapply(models.lvl1, f))), as.numeric)
  # Meta level prediction
  models.meta = models[(length(.model$task.desc$target) + 1):(2 * length(.model$task.desc$target))]
  newdata.meta = data.frame(.newdata, pred.lvl1)
  f = if (.learner$predict.type == "response") {
    function(m, tn) as.logical(getPredictionResponse(predict(m, newdata = newdata.meta[, which(colnames(newdata.meta) != tn)], subset = .subset, ...)))
  } else {
    function(m, tn) getPredictionProbabilities(predict(m, newdata = newdata.meta[, which(colnames(newdata.meta) != tn)], subset = .subset, ...), cl = "TRUE")
  }
  mapply(f, models.meta, .model$task.desc$target)
}
