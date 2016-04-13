#' @title Use stacking method (stacked generalization) to create a multilabel learner.
#'
#' @description
#' Every learner which is implemented in mlr and which supports binary
#' classification can be converted to a wrapped stacking multilabel learner.
#' Stacking trains a binary classifier for each label using predicted label information of all labels (including the target label)
#' as additional features (by cross validation).
#' During prediction these labels need are obtained by the binary relevance method using the same binary learner.
#'
#' Models can easily be accessed via \code{\link{getLearnerModel}}.
#'
#' @param  cv.folds The number of folds for the inner cross validation to predict labels for the augmented feature space. Default is \code{2}.
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
#' lrn = makeMultilabelStackingWrapper("classif.rpart")
#' lrn = setPredictType(lrn, "prob")
#' # train, predict and evaluate
#' mod = train(lrn, task)
#' pred = predict(mod, task)
#' p = performance(pred)
#' performance(pred, measure = hamloss)
#' getMultilabelBinaryPerformances(pred, measures = list(mmce, auc))
#' # above works also with predictions from resample!
makeMultilabelStackingWrapper = function(learner, cv.folds = 2) {
  learner = checkLearner(learner, type = "classif")
  hasLearnerProperties(learner, "twoclass")
  id = paste("multilabel", learner$id, sep = ".")
  packs = learner$package
  x = makeHomogeneousEnsemble(id, learner$type, learner, packs, learner.subclass = "MultilabelStackingWrapper", model.subclass = "MultilabelStackingModel")
  x$type = "multilabel"
  x$cv.folds = cv.folds
  return(x)
}

#' @export
trainLearner.MultilabelStackingWrapper = function(.learner, .task, .subset, .weights = NULL, ...) {
  targets = getTaskTargetNames(.task)
  .task = subsetTask(.task, subset = .subset)
  data = getTaskData(.task)
  # train level 1 learners
  modelsLvl1 = getLearnerModel(train(makeMultilabelBinaryRelevanceWrapper(.learner$next.learner), .task, weights = .weights))
  # predict labels
  f = function(tn) {
    data2 = dropNamed(data, setdiff(targets, tn))
    ctask = makeClassifTask(id = tn, data = data2, target = tn)
    r = resample(.learner$next.learner, ctask, weights = .weights,
      makeResampleDesc("CV", iters = .learner$cv.folds), show.info = FALSE)
    as.numeric(as.logical(r$pred$data[order(r$pred$data$id), ]$response)) #did not use getPredictionResponse, because of ordering
  }
  predLabels = sapply(targets, f)
  # train meta level learners
  g = function(tn) {
    dataMeta = dropNamed(data.frame(data, predLabels), setdiff(targets, tn))    
    ctask = makeClassifTask(id = tn, data = dataMeta, target = tn)
    train(.learner$next.learner, ctask, weights = .weights)
  }
  modelsMeta = lapply(targets, g)
  makeHomChainModel(.learner, c(modelsLvl1, modelsMeta))
}

#' @export
predictLearner.MultilabelStackingWrapper = function(.learner, .model, .newdata, ...) {
  models = getLearnerModel(.model, more.unwrap = FALSE)
  # Level 1 prediction (binary relevance)
  modelsLvl1 = models[1:length(.model$task.desc$target)]
  f = if (.learner$predict.type == "response") {
    function(m) as.logical(getPredictionResponse(predict(m, newdata = .newdata, ...)))
  } else {
    function(m) getPredictionProbabilities(predict(m, newdata = .newdata, ...), cl = "TRUE")
  }
  if (.learner$predict.type == "response") {
    predLvl1 = sapply(data.frame(asMatrixCols(lapply(modelsLvl1, f))), as.numeric)
  } else {
    predLvl1 = data.frame(asMatrixCols(lapply(modelsLvl1, f)))
  }
  colnames(predLvl1) = paste(.model$task.desc$target, ".1", sep = "")
  # Meta level prediction
  modelsMeta = models[(length(.model$task.desc$target) + 1):(2 * length(.model$task.desc$target))]
  nd = data.frame(.newdata, predLvl1)
  g = if (.learner$predict.type == "response") {
    function(m) as.logical(getPredictionResponse(predict(m, newdata = nd, ...)))
  } else {
    function(m) getPredictionProbabilities(predict(m, newdata = nd, ...), cl = "TRUE")
  }
  asMatrixCols(lapply(modelsMeta, g), col.names = .model$task.desc$target)
}
