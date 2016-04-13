#' @title Use classifier chains method (CC) to create a multilabel learner.
#'
#' @description
#' Every learner which is implemented in mlr and which supports binary
#' classification can be converted to a wrapped classifier chains multilabel learner.
#' CC trains a binary classifier for each label following a given order. In training phase,
#' the feature space of each classifier is extended with true label information of all previous
#' labels in the chain. During the prediction phase, when true labels are not available, they are
#' replaced by predicted labels.
#'
#' Models can easily be accessed via \code{\link{getLearnerModel}}.
#'
#' @param order The chain order. E.g. for \code{m} labels, this must be a permutation of \code{1:m}. Default is \code{"random"}, i.e. \code{sample(1:m)}.
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
#' lrn = makeMultilabelClassifierChainsWrapper("classif.rpart", order = 1:3)
#' lrn = setPredictType(lrn, "prob")
#' # train, predict and evaluate
#' mod = train(lrn, task)
#' pred = predict(mod, task)
#' p = performance(pred)
#' performance(pred, measure = hamloss)
#' getMultilabelBinaryPerformances(pred, measures = list(mmce, auc))
#' # above works also with predictions from resample!
makeMultilabelClassifierChainsWrapper = function(learner, order = "random") {
  learner = checkLearner(learner, type = "classif")
  hasLearnerProperties(learner, "twoclass")
  id = paste("multilabel", learner$id, sep = ".")
  packs = learner$package
  x = makeHomogeneousEnsemble(id, learner$type, learner, packs,
    learner.subclass = "MultilabelClassifierChainsWrapper", 
    model.subclass = "MultilabelClassifierChainsModel")
  x$type = "multilabel"
  x$order = order
  return(x)
}

#' @export
trainLearner.MultilabelClassifierChainsWrapper = function(.learner, .task, .subset, .weights = NULL, ...){
  if (.learner$order[1] == "random") {
    order = sample(1:length(.task$task.desc$target)) #random order
  } else {
    order = .learner$order
  }
  if (length(order) != length(getTaskTargetNames(.task))) {
    stopf("order length does not match number of targets!")  
  }
  targets = getTaskTargetNames(.task)
  .task = subsetTask(.task, subset = .subset)
  data = getTaskData(.task)
  models = namedList(targets[order])
  chained_targets = targets
  for (tn in targets[order]) {
    chained_targets = setdiff(chained_targets, tn)
    data2 = dropNamed(data, chained_targets)
    index = which(names(data2) %in% setdiff(targets, tn))
    if (length(data2[, index]) != 0) {  #convert augmented features into 0/1 variables, since boolean doesn't work
      data2[, index] = sapply(data2[, index], as.numeric) 
    }
    ctask = makeClassifTask(id = tn, data = data2, target = tn)
    models[[tn]] = train(.learner$next.learner, ctask, weights = .weights)
  }
  makeHomChainModel(.learner, models)
}
#' @export
predictLearner.MultilabelClassifierChainsWrapper = function(.learner, .model, .newdata, ...) {
  models = getLearnerModel(.model, more.unwrap = FALSE)
  predmatrix = matrix(ncol = length(models), nrow = nrow(.newdata), dimnames = list(NULL, names(models)))
  if (.learner$predict.type == "response") {
    for (tn in names(models)) {
      predmatrix[, tn] = as.logical(getPredictionResponse(predict(models[[tn]], newdata = .newdata, ...)))
      .newdata[paste(tn)] = as.numeric(predmatrix[, tn])
    }
  } else {
    for (tn in names(models)) {
      predmatrix[, tn] = getPredictionProbabilities(predict(models[[tn]], newdata = .newdata, ...), cl = "TRUE")
      .newdata[paste(tn)] = predmatrix[, tn]
    }
  }
  predmatrix[, .model$task.desc$class.levels] #bring labels back in original order
}

