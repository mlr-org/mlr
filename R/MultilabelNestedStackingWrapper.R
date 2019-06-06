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
#' Models can easily be accessed via [getLearnerModel].
#'
#' @template arg_learner
#' @template arg_multilabel_order
#' @template arg_multilabel_cvfolds
#' @template ret_learner
#' @references
#' Montanes, E. et al. (2013),
#' *Dependent binary relevance models for multi-label classification*
#' Artificial Intelligence Center, University of Oviedo at Gijon, Spain.
#' @family wrapper
#' @family multilabel
#' @export
#' @example inst/examples/MultilabelWrapper.R
makeMultilabelNestedStackingWrapper = function(learner, order = NULL, cv.folds = 2) {

  learner = checkLearner(learner, type = "classif", props = "twoclass")
  id = stri_paste("multilabel.nestedStacking", getLearnerId(learner), sep = ".")
  packs = getLearnerPackages(learner)
  type = getLearnerType(learner)
  x = makeHomogeneousEnsemble(id, type, learner, packs,
    learner.subclass = "MultilabelNestedStackingWrapper",
    model.subclass = "MultilabelNestedStackingModel")
  x$type = "multilabel"
  x$order = order
  x$cv.folds = cv.folds
  return(x)
}

#' @export
trainLearner.MultilabelNestedStackingWrapper = function(.learner, .task, .subset = NULL, .weights = NULL, ...) {

  if (is.null(.learner$order)) {
    order = sample(getTaskTargetNames(.task)) # random order
  } else {
    order = .learner$order
  }
  assertSetEqual(order, getTaskTargetNames(.task))
  targets = getTaskTargetNames(.task)
  .task = subsetTask(.task, subset = .subset)
  data = getTaskData(.task)
  models = namedList(order)
  data.nst = dropNamed(data, targets)
  chained.targets = targets
  for (i in seq_along(targets)) {
    tn = order[i]
    if (i >= 2) {
      tnprevious = order[i - 1]
      data2 = data.frame(data.nst, data[tnprevious]) # for inner resampling to produce predicted labels
      innertask = makeClassifTask(id = tnprevious, data = data2, target = tnprevious)
      rdesc = makeResampleDesc("CV", iters = .learner$cv.folds)
      r = resample(.learner$next.learner, innertask, rdesc, weights = .weights, show.info = FALSE)
      predlabel = as.numeric(as.logical(r$pred$data[order(r$pred$data$id), ]$response)) # did not use getPredictionResponse, because of ordering
      data2 = data.frame(data.nst, data[tn])
      data2[[tnprevious]] = predlabel
      data.nst[[tnprevious]] = predlabel
    } else {
      data2 = dropNamed(data, setdiff(chained.targets, tn))
    }
    ctask = makeClassifTask(id = tn, data = data2, target = tn)
    models[[tn]] = train(.learner$next.learner, ctask, weights = .weights)
  }
  makeHomChainModel(.learner, models)
}

#' @export
predictLearner.MultilabelNestedStackingWrapper = function(.learner, .model, .newdata, .subset = NULL, ...) {
  models = getLearnerModel(.model, more.unwrap = FALSE)
  predmatrix = matrix(ncol = length(models), nrow = nrow(.newdata), dimnames = list(NULL, names(models)))
  if (.learner$predict.type == "response") {
    for (tn in names(models)) {
      predmatrix[, tn] = as.logical(getPredictionResponse(predict(models[[tn]], newdata = .newdata, subset = .subset, ...)))
      .newdata[tn] = as.numeric(predmatrix[, tn])
    }
  } else {
    for (tn in names(models)) {
      predmatrix[, tn] = getPredictionProbabilities(predict(models[[tn]], newdata = .newdata, subset = .subset, ...), cl = "TRUE")
      .newdata[tn] = predmatrix[, tn]
    }
  }
  predmatrix[, .model$task.desc$class.levels] # bring labels back in original order
}
