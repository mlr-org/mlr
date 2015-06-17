
#' @export
makeMultilabelWrapper = function(learner) {
  learner = checkLearner(learner, type = "classif")
  id = paste("multilabel", learner$id, sep = ".")
  packs = learner$package
  x = makeHomogeneousEnsemble(id, learner$type, learner, packs,
    learner.subclass = "MultilabelWrapper", model.subclass = "MultilabelModel")
  x$type = "multilabel"
  return(x)
}


#' @export
trainLearner.MultilabelWrapper = function(.learner, .task, .subset, .weights = NULL,...) {
  targets = getTaskTargetNames(.task)
  .task = subsetTask(.task, subset = .subset)
  data = getTaskData(.task)
  models = vector("list", length(targets))
  for (tn in seq_along(targets)) {
    data2 = dropNamed(data, setdiff())
    ctask = makeClassifTask(id = tn , data = data2, tn)
    models[[tn]] = train(.learner$next.learner, ctask, weights = .weights)
  }
  makeHomChainModel(.learner, models)
}

#' @export
print.MultilabelModel = function(x, ...) {
  cat(
    "Model for id = ", x$learner$id, " class = ", getClass1(x$learner), "\n",
    "Trained on obs: ", length(x$subset), "\n",
    "Used features: ", length(x$features), "\n",
    "Hyperparameters: ", getHyperParsString(x$learner), "\n",
    sep = ""
  )
}

#' @export
predictLearner.MultilabelWrapper = function(.learner, .model, .newdata, ...) {
  models = getHomogeneousEnsembleModels(.model, learner.models = FALSE)
  g = if (.learner$type == "classif") as.character else identity
  p = asMatrixCols(lapply(models, function(m) {
    nd = .newdata[, m$features, drop = FALSE]
    g(predict(m, newdata = nd, ...)$data$response)
  }))


  pred = list()
  for (i in 1:length(.model$learner.model)) {
    model = .model$learner.model[[i]]
    pred[[i]] = predict(object = model, newdata = .newdata)$data
    if (.learner$predict.type == "prob") {
      pred[[i]] = pred[[i]][-ncol(pred[[i]])]
      names(pred[[i]]) = substr(names(pred[[i]]), 6, nchar(names(pred[[i]])))
    }
  }
  names(pred) = names(.model$learner.model)
  return(pred)
}
