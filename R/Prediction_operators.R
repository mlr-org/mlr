#' @export
as.data.frame.Prediction = function(x, row.names = NULL, optional = FALSE,...) {
  x$data
}

#' Get probabilities for some classes.
#'
#' @template arg_pred
#' @param cl [\code{character}]\cr
#'   Names of classes.
#'   Default is either all classes for multi-class problems or the positive class for binary classification.
#' @return [\code{data.frame}] with numerical columns or a numerical vector if length of \code{cl} is 1.
#'   Order of columns is defined by \code{cl}.
#' @export
#' @family predict
#' @examples
#' task = makeClassifTask(data = iris, target = "Species")
#' lrn = makeLearner("classif.lda", predict.type = "prob")
#' mod = train(lrn, task)
#' # predict probabilities
#' pred = predict(mod, newdata = iris)
#'
#' # Get probabilities for all classes
#' head(getProbabilities(pred))
#'
#' # Get probabilities for a subset of classes
#' head(getProbabilities(pred, c("setosa", "virginica")))
getProbabilities = function(pred, cl) {
  assertClass(pred, classes = "Prediction")
  if (pred$task.desc$type != "classif")
    stop("Prediction was not generated from a ClassifTask!")
  if (missing(cl)) {
    if (length(pred$task.desc$class.levels) == 2L)
      cl = pred$task.desc$positive
    else
      cl = pred$task.desc$class.levels
  } else {
    assertCharacter(cl, any.missing = FALSE)
  }
  if (pred$predict.type != "prob")
    stop("Probabilities not present in Prediction object!")
  cns = colnames(pred$data)
  cl2 = paste("prob", cl, sep = ".")
  if (!all(cl2 %in% cns))
    stopf("Trying to get probabilities for nonexistant classes: %s", collapse(cl))
  y = pred$data[, cl2]
  if (length(cl) > 1L)
    colnames(y) = cl
  return(y)
}

#c.Prediction = function(...) {
#	preds = list(...)
#	id = Reduce(c, lapply(preds, function(x) x@id))
#	response = Reduce(c, lapply(preds, function(x) x@response))
#	target = Reduce(c, lapply(preds, function(x) x@target))
#	weights = Reduce(c, lapply(preds, function(x) x@weights))
#	prob = Reduce(rbind, lapply(preds, function(x) x@prob))
#	return(new("Prediction", task.desc = preds[[1]]@desc, id = id, response = response, target = target, weights = weights, prob = prob));
#}
