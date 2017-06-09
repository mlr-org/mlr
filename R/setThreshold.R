#' @title Set threshold of prediction object.
#'
#' @description
#' Set threshold of prediction object for classification or multilabel classification.
#' Creates corresponding discrete class response for the newly set threshold.
#' For binary classification: The positive class is predicted if the probability value exceeds the threshold.
#' For multiclass: Probabilities are divided by corresponding thresholds and the class with maximum resulting value is selected.
#' The result of both are equivalent if in the multi-threshold case the values are greater than 0 and sum to 1.
#' For multilabel classification: A label is predicted (with entry \code{TRUE}) if a probability matrix entry
#' exceeds the threshold of the corresponding label.
#'
#' @template arg_pred
#' @param threshold [\code{numeric}]\cr
#'   Threshold to produce class labels. Has to be a named vector, where names correspond to class labels.
#'   Only for binary classification it can be a single numerical threshold for the positive class.
#' @return [\code{\link{Prediction}}] with changed threshold and corresponding response.
#' @export
#' @seealso \code{\link{predict.WrappedModel}}
#' @examples
#' # create task and train learner (LDA)
#' task = makeClassifTask(data = iris, target = "Species")
#' lrn = makeLearner("classif.lda", predict.type = "prob")
#' mod = train(lrn, task)
#'
#' # predict probabilities and compute performance
#' pred = predict(mod, newdata = iris)
#' performance(pred, measures = mmce)
#' head(as.data.frame(pred))
#'
#' # adjust threshold and predict probabilities again
#' threshold = c(setosa = 0.4, versicolor = 0.3, virginica = 0.3)
#' pred = setThreshold(pred, threshold = threshold)
#' performance(pred, measures = mmce)
#' head(as.data.frame(pred))
setThreshold = function(pred, threshold) {
  # dont check for NAs in response, this will get overwritten anyway.
  # and object might not be constructed in full when we call this in Prediction
  checkPrediction(pred, task.type = c("classif", "multilabel"), predict.type = "prob", no.na = FALSE)
  assertNumeric(threshold, any.missing = FALSE)
  td = pred$task.desc
  ttype = td$type
  levs = td$class.levels
  if (length(levs) == 2L && is.numeric(threshold) && length(threshold) == 1L) {
    threshold = c(threshold, 1 - threshold)
    names(threshold) = c(td$positive, td$negative)
  }
  if (length(threshold) > 1L && !setequal(levs, names(threshold)))
    stop("Threshold names must correspond to classes!")
  p = getPredictionProbabilities(pred, cl = levs)
  # resort so we have same order in threshold and p
  threshold = threshold[levs]
  if (ttype == "classif") {
    # divide all rows by threshold then get max el
    p = sweep(as.matrix(p), MARGIN = 2, FUN = "/", threshold)
    # 0 / 0 can produce NaNs. For a 0 threshold we always want Inf weight for that class
    p[is.nan(p)] = Inf
    ind = getMaxIndexOfRows(p)
    pred$data$response = factor(ind, levels = seq_along(levs), labels = levs)
  } else if (ttype == "multilabel") {
    # substract threshold from every entry, then check if > 0, then set response level
    p = sweep(as.matrix(p), MARGIN = 2, FUN = "-", threshold)
    i = stri_paste("response.", levs)
    pred$data[, i] = p > 0
  }
  pred$threshold = threshold
  return(pred)
}

