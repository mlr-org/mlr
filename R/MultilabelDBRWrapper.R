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
#' Models can easily be accessed via \code{\link{getLearnerModel}}.
#'
#' @template arg_learner
#' @template ret_learner
#' @references
#' Montanes, E. et al. (2013)
#' \emph{Dependent binary relevance models for multi-label classification}
#' Artificial Intelligence Center, University of Oviedo at Gijon, Spain.
#' @family wrapper
#' @family multilabel
#' @export
#' @examples
#' d = getTaskData(yeast.task)
#' # drop some labels so example runs faster
#' d = d[, c(1:3, 15:117)]
#' task = makeMultilabelTask(data = d, target = c("label1", "label2", "label3"))
#' lrn = makeMultilabelDBRWrapper("classif.rpart")
#' lrn = setPredictType(lrn, "prob")
#' # train, predict and evaluate
#' mod = train(lrn, task)
#' pred = predict(mod, task)
#' p = performance(pred)
#' performance(pred, measure = hamloss)
#' getMultilabelBinaryPerformances(pred, measures = list(mmce, auc))
#' # above works also with predictions from resample!
makeMultilabelDBRWrapper = function(learner) {
  learner = checkLearner(learner, type = "classif")
  hasLearnerProperties(learner, "twoclass")
  id = paste("multilabel", learner$id, sep = ".")
  packs = learner$package
  x = makeHomogeneousEnsemble(id, learner$type, learner, packs,
    learner.subclass = "MultilabelDBRWrapper",
    model.subclass = "MultilabelDBRModel")
  x$type = "multilabel"
  return(x)
}

#' @export
trainLearner.MultilabelDBRWrapper = function(.learner, .task, .subset, .weights = NULL, ...) {
  targets = getTaskTargetNames(.task)
  .task = subsetTask(.task, subset = .subset)
  data = getTaskData(.task)
  # train level 1 learners (binary relevance)
  modelsLvl1 = getLearnerModel(train(makeMultilabelBinaryRelevanceWrapper(.learner$next.learner), .task, weights = .weights))
  # train meta level learners
  modelsMeta = namedList(targets)
  for (tn in targets) {
    data2Meta = dropNamed(data, setdiff(targets, tn))
    trueLabelData = sapply(data.frame(data[, setdiff(targets, tn)]), as.numeric)
    colnames(trueLabelData) = setdiff(targets, tn)
    data2Meta = data.frame(data2Meta, trueLabelData)
    ctask = makeClassifTask(id = tn, data = data2Meta, target = tn)
    modelsMeta[[tn]] = train(.learner$next.learner, ctask, weights = .weights)
  }
  makeHomChainModel(.learner, c(modelsLvl1, modelsMeta))
}

#' @export
predictLearner.MultilabelDBRWrapper = function(.learner, .model, .newdata, ...) {
  models = getLearnerModel(.model, more.unwrap = FALSE)
  # Level 1 prediction (binary relevance)
  modelsLvl1 = models[1:length(.model$task.desc$target)]
  f = if (.learner$predict.type == "response") {
    function(m) getPredictionResponse(predict(m, newdata = .newdata, ...))
  } else {
    function(m) getPredictionProbabilities(predict(m, newdata = .newdata, ...), cl = "TRUE")
  }
  predLvl1 = sapply(data.frame(asMatrixCols(lapply(modelsLvl1, f))), as.numeric)
  # Meta level prediction
  modelsMeta = models[(length(.model$task.desc$target) + 1):(2 * length(.model$task.desc$target))]
  newdataMeta = data.frame(.newdata, predLvl1)
  f = if (.learner$predict.type == "response") {
    function(m, tn) as.logical(getPredictionResponse(predict(m, newdata = newdataMeta[, which(colnames(newdataMeta) != tn)], ...)))
  } else {
    function(m, tn) getPredictionProbabilities(predict(m, newdata = newdataMeta[, which(colnames(newdataMeta) != tn)], ...), cl = "TRUE")
  }
  mapply(f, modelsMeta, .model$task.desc$target)
}
