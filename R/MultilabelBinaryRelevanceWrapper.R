#' @title Use binary relevance method to create a multilabel learner.
#'
#' @description
#' Every learner which is implemented in mlr and which supports binary
#' classification can be converted to a wrapped binary relevance multilabel learner.
#' The multilabel classification problem is converted into simple binary classifications
#' for each label/target on which the binary learner is applied.
#'
#' Models can easily be accessed via \code{\link{getLearnerModel}}.
#'
#' Note that it does not make sense to set a threshold in the used base \code{learner}
#' when you predict probabilities.
#' On the other hand, it can make a lot of sense, to call \code{\link{setThreshold}}
#' on the \code{MultilabelBinaryRelevanceWrapper} for each label indvidually;
#' Or to tune these thresholds with \code{\link{tuneThreshold}}; especially when you face very
#' unabalanced class distributions for each binary label.
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
#' \donttest{
#' d = getTaskData(yeast.task)
#' # drop some labels so example runs faster
#' d = d[, c(1:3, 15:117)]
#' task = makeMultilabelTask(data = d, target = c("label1", "label2", "label3"))
#' lrn = makeMultilabelBinaryRelevanceWrapper("classif.rpart")
#' lrn = setPredictType(lrn, "prob")
#' # train, predict and evaluate
#' mod = train(lrn, yeast.task)
#' pred = predict(mod, yeast.task)
#' p = performance(pred)
#' performance(pred, measure = hamloss)
#' getMultilabelBinaryPerformances(pred, measures = list(mmce, auc))
#' # above works also with predictions from resample!
#' }
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
  models = getLearnerModel(.model, more.unwrap = FALSE)
  f = if (.learner$predict.type == "response")
    function(m) as.logical(predict(m, newdata = .newdata, ...)$data$response)
  else
    function(m) predict(m, newdata = .newdata, ...)$data$prob.TRUE
  asMatrixCols(lapply(models, f))
}
