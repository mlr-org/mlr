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
#' Models can easily be accessed via [getLearnerModel].
#'
#' @template arg_learner
#' @template arg_multilabel_order
#' @template ret_learner
#' @references
#' Montanes, E. et al. (2013)
#' *Dependent binary relevance models for multi-label classification*
#' Artificial Intelligence Center, University of Oviedo at Gijon, Spain.
#' @family wrapper
#' @family multilabel
#' @export
#' @example inst/examples/MultilabelWrapper.R
makeMultilabelClassifierChainsWrapper = function(learner, order = NULL) {
  learner = checkLearner(learner, type = "classif", props = "twoclass")
  id = stri_paste("multilabel.classifierChains", getLearnerId(learner), sep = ".")
  packs = getLearnerPackages(learner)
  type = getLearnerType(learner)
  x = makeHomogeneousEnsemble(id, type, learner, packs,
    learner.subclass = "MultilabelClassifierChainsWrapper",
    model.subclass = "MultilabelClassifierChainsModel")
  x$type = "multilabel"
  x$order = order
  return(x)
}

#' @export
trainLearner.MultilabelClassifierChainsWrapper = function(.learner, .task, .subset = NULL, .weights = NULL, ...) {

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
  chained.targets = targets
  for (tn in order) {
    chained.targets = setdiff(chained.targets, tn)
    data2 = dropNamed(data, chained.targets)
    index = which(names(data2) %in% setdiff(targets, tn))
    if (length(index) != 0) { # convert augmented features into 0/1 variables, since boolean doesn't work
      data2[, index] = sapply(data2[, index], as.numeric)
    }
    ctask = makeClassifTask(id = tn, data = data2, target = tn)
    models[[tn]] = train(.learner$next.learner, ctask, weights = .weights)
  }
  makeHomChainModel(.learner, models)
}

#' @export
predictLearner.MultilabelClassifierChainsWrapper = function(.learner, .model, .newdata, .subset = NULL, ...) {
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
