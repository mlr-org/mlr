
#' @export
makeMultilabelBinaryRelevanceWrapper = function(learner) {
  learner = checkLearner(learner, type = "classif")
  id = paste("multilabel", learner$id, sep = ".")
  packs = learner$package
  x = makeHomogeneousEnsemble(id, learner$type, learner, packs,
    learner.subclass = "MultilabelBinaryRelevanceWrapper", model.subclass = "MultilabelBinaryRelevanceModel")
  x$type = "multilabel"
  return(x)
}


#' @export
trainLearner.MultilabelBinaryRelevanceWrapper = function(.learner, .task, .subset, .weights = NULL,...) {
  targets = getTaskTargetNames(.task)
  .task = subsetTask(.task, subset = .subset)
  data = getTaskData(.task)
  models = namedList(targets)
  for (tn in targets) {
    data2 = dropNamed(data, setdiff(targets, tn))
    ctask = makeClassifTask(id = tn, data = data2, target = tn)
    models[[tn]] = train(.learner$next.learner, ctask, weights = .weights)
  }
  makeHomChainModel(.learner, models)
}

#' @export
predictLearner.MultilabelBinaryRelevanceWrapper = function(.learner, .model, .newdata, ...) {
  models = getHomogeneousEnsembleModels(.model, learner.models = FALSE)
  f = if (.learner$predict.type == "response")
    function(m) as.logical(predict(m, newdata = .newdata, ...)$data$response)
  else
    function(m) predict(m, newdata = .newdata, ...)$data$prob.TRUE
  asMatrixCols(lapply(models, f))
}
