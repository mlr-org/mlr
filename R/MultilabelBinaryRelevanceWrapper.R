#' @title Use binary relevance method to create a multilabel learner.
#'
#' @description
#' Every learner which is implemented in mlr and which supports binary
#' classification can be converted to a wrapped binary relevance multilabel learner.
#' The multilabel classification problem is converted into simple binary classifications
#' for each label/target on which the binary learner is applied.
#'
#' @template arg_learner
#' @template ret_learner
#' @references
#' Tsoumakas, G., & Katakis, I. (2006)
#' \emph{Multi-label classification: An overview.} 
#' Dept. of Informatics, Aristotle University of Thessaloniki, Greece.
#' @family wrapper 
#' @family multilabel
#' @export
#' @examples
#' labels = colnames(yeast)[1:14]
#' yeast.task = makeMultilabelTask(id = "multi", data = yeast, target = labels)
#' lrn = makeMultilabelBinaryRelevanceWrapper("classif.rpart")
#' # train, predict and evaluate
#' mod = train(lrn, yeast.task)
#' pred = predict(mod, yeast.task)
#' p = performance(pred)
#' # with newdata
#' pred = predict(mod, newdata = yeast[1:10,])
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
