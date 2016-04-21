#' @title Use nested stacking method to create a multilabel learner.
#'
#' @description
#' Every learner which is implemented in mlr and which supports binary
#' classification can be converted to a wrapped nested stacking multilabel learner.
#' Nested stacking trains a binary classifier for each label following a given order. In training phase,
#' the feature space of each classifier is extended with predicted label information (by cross validation) 
#' of all previous labels in the chain. 
#' During the prediction phase, predicted labels are obtained by the classifiers, which have been learned on 
#' all training data.
#'
#' Models can easily be accessed via \code{\link{getLearnerModel}}.
#'
#' @param order The chain order. E.g. for \code{m} labels, this must be a permutation of \code{1:m}. Default is \code{"random"}, i.e. \code{sample(1:m)}.
#' @param  cv.folds The number of folds for the inner cross validation method to predict labels for the augmented feature space. Default is \code{2}.
#' @template arg_learner
#' @template ret_learner
#' @references
#' Montanes, E. et al. (2013),
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
#' lrn = makeMultilabelNestedStackingWrapper("classif.rpart", order = 1:3)
#' lrn = setPredictType(lrn, "prob")
#' # train, predict and evaluate
#' mod = train(lrn, task)
#' pred = predict(mod, task)
#' p = performance(pred)
#' performance(pred, measure = hamloss)
#' getMultilabelBinaryPerformances(pred, measures = list(mmce, auc))
#' # above works also with predictions from resample!
makeMultilabelNestedStackingWrapper = function(learner, order = "random", cv.folds = 2) {
  learner = checkLearner(learner, type = "classif")
  hasLearnerProperties(learner, "twoclass")
  id = paste("multilabel", learner$id, sep = ".")
  packs = learner$package
  x = makeHomogeneousEnsemble(id, learner$type, learner, packs,
    learner.subclass = "MultilabelNestedStackingWrapper",
    model.subclass = "MultilabelNestedStackingModel")
  x$type = "multilabel"
  x$order = order
  x$cv.folds = cv.folds
  return(x)
}
#' @export
trainLearner.MultilabelNestedStackingWrapper = function(.learner, .task, .subset, .weights = NULL, ...) {
  if (.learner$order[1] == "random") {
    order = sample(1:length(getTaskTargetNames(.task))) #random order
  } else {
    order = .learner$order
  }
  if (sort(order) != 1:length(getTaskTargetNames(.task))) {
    stopf("order does not match number of targets!")
  }  
  targets = getTaskTargetNames(.task)
  .task = subsetTask(.task, subset = .subset)
  data = getTaskData(.task)
  models = namedList(targets[order])
  dataNST = dropNamed(data, targets)
  chained_targets = targets
  for (i in 1:length(targets)) {
    tn = targets[order][i]
    if (i >= 2) {
      tnprevious = targets[order][i-1]
      data2 = data.frame(dataNST, data[tnprevious]) #for inner resampling to produce predicted labels
      innertask = makeClassifTask(id = tnprevious, data = data2, target = tnprevious)
      r = resample(.learner$next.learner, innertask, makeResampleDesc("CV", iters = .learner$cv.folds), weights = .weights, show.info = FALSE)
      predlabel = as.numeric(as.logical(r$pred$data[order(r$pred$data$id), ]$response)) #did not use getPredictionResponse, because of ordering
      data2 = data.frame(dataNST, data[tn])
      data2[[tnprevious]] = predlabel
      dataNST[[tnprevious]] = predlabel
    } else {
      data2 = dropNamed(data, setdiff(chained_targets, tn))
    }
    ctask = makeClassifTask(id = tn, data = data2, target = tn)
    models[[tn]] = train(.learner$next.learner, ctask, weights = .weights)
  }
  makeHomChainModel(.learner, models)
}
#' @export
predictLearner.MultilabelNestedStackingWrapper = function(.learner, .model, .newdata, ...) {
  models = getLearnerModel(.model, more.unwrap = FALSE)
  predmatrix = matrix(ncol = length(models), nrow = nrow(.newdata), dimnames = list(NULL, names(models)))
  if (.learner$predict.type == "response") {
    for (tn in names(models)) {
      predmatrix[, tn] = as.logical(getPredictionResponse(predict(models[[tn]], newdata = .newdata, ...)))
      .newdata[tn] = as.numeric(predmatrix[, tn])
    }
  } else {
    for (tn in names(models)) {
      predmatrix[, tn] = getPredictionProbabilities(predict(models[[tn]], newdata = .newdata, ...), cl = "TRUE")
      .newdata[tn] = predmatrix[, tn]
    }
  }
  predmatrix[, .model$task.desc$class.levels] #bring labels back in original order
}

