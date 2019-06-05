#' @title Use binary relevance method to create a multilabel learner.
#'
#' @description
#' Every learner which is implemented in mlr and which supports binary
#' classification can be converted to a wrapped binary relevance multilabel learner.
#' The multilabel classification problem is converted into simple binary classifications
#' for each label/target on which the binary learner is applied.
#'
#' Models can easily be accessed via [getLearnerModel].
#'
#' Note that it does not make sense to set a threshold in the used base `learner`
#' when you predict probabilities.
#' On the other hand, it can make a lot of sense, to call [setThreshold]
#' on the `MultilabelBinaryRelevanceWrapper` for each label indvidually;
#' Or to tune these thresholds with [tuneThreshold]; especially when you face very
#' unabalanced class distributions for each binary label.
#'
#' @template arg_learner
#' @template ret_learner
#' @references
#' Tsoumakas, G., & Katakis, I. (2006)
#' *Multi-label classification: An overview.*
#' Dept. of Informatics, Aristotle University of Thessaloniki, Greece.
#' @family wrapper
#' @family multilabel
#' @export
#' @example inst/examples/MultilabelWrapper.R
makeMultilabelBinaryRelevanceWrapper = function(learner) {
  learner = checkLearner(learner, type = "classif")
  id = stri_paste("multilabel.binaryRelevance", getLearnerId(learner), sep = ".")
  packs = getLearnerPackages(learner)
  type = getLearnerType(learner)
  x = makeHomogeneousEnsemble(id, type, learner, packs,
    learner.subclass = "MultilabelBinaryRelevanceWrapper", model.subclass = "MultilabelBinaryRelevanceModel")
  x$type = "multilabel"
  return(x)
}

#' @export
trainLearner.MultilabelBinaryRelevanceWrapper = function(.learner, .task, .subset = NULL, .weights = NULL, ...) {
  targets = getTaskTargetNames(.task)
  .task = subsetTask(.task, subset = .subset)
  parallelLibrary("mlr", master = FALSE, level = "mlr.ensemble", show.info = FALSE)
  exportMlrOptions(level = "mlr.ensemble")
  models = parallelMap(
    doMultilabelBinaryRelevanceTrainIteration, tn = targets,
    more.args = list(weights = .weights, learner = .learner$next.learner, task = .task),
    level = "mlr.ensemble")
  names(models) = targets
  makeHomChainModel(.learner, models)
}

doMultilabelBinaryRelevanceTrainIteration = function(tn, learner, task, weights) {
  setSlaveOptions()
  data = getTaskData(task)
  task = makeClassifTask(id = tn, data = dropNamed(data, setdiff(getTaskTargetNames(task), tn)), target = tn)
  train(learner, task, weights = weights)
}


#' @export
predictLearner.MultilabelBinaryRelevanceWrapper = function(.learner, .model, .newdata, .subset = NULL, ...) {
  models = getLearnerModel(.model, more.unwrap = FALSE)
  f = if (.learner$predict.type == "response") {
    function(m) as.logical(getPredictionResponse(predict(m, newdata = .newdata, subset = .subset, ...)))
  } else {
    function(m) getPredictionProbabilities(predict(m, newdata = .newdata, subset = .subset, ...), cl = "TRUE")
  }
  asMatrixCols(lapply(models, f))
}
