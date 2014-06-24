#' Set threshold of prediction object.
#'
#' Set threshold of prediction object for classification.
#' Creates corresponding discrete class response for the newly set threshold.
#' For binary classification: The positive class is predicted if the probability value exceeds the threshold.
#' For multiclass: Probabilities are divided by corresponding thresholds and the class with maximum resulting value is selected.
#' The result of both are equivalent if in the multi-threshold case the values are greater than 0 and sum to 1.
#'
#' @template arg_pred
#' @param threshold [\code{numeric}]\cr
#'   Threshold to produce class labels. Has to be a named vector, where names correspond to class labels.
#'   Only if \code{pred} is a prediction object resulting from binary classification
#'   it can be a single numerical threshold for the positive class.
#' @return [\code{\link{Prediction}}] with changed threshold and corresponding response.
#' @export
#' @seealso \code{\link{predict.WrappedModel}}
#' @examples
#' ## create task and train learner (LDA)
#' task <- makeClassifTask(data = iris, target = "Species")
#' lrn <- makeLearner("classif.lda", predict.type = "prob")
#' mod <- train(lrn, task)
#'
#' ## predict probabilities and compute performance
#' pred <- predict(mod, newdata = iris)
#' performance(pred, measures = mmce)
#' head(as.data.frame(pred))

#' ## adjust threshold and predict probabilities again
#' threshold <- c(setosa = 0.4, versicolor = 0.3, virginica = 0.3)
#' pred <- setThreshold(pred, threshold = threshold)
#' performance(pred, measures = mmce)
#' head(as.data.frame(pred))
setThreshold = function(pred, threshold) {
  checkArg(pred, "Prediction")
  checkArg(threshold, "numeric", na.ok = FALSE)
  td = pred$task.desc
  if (td$type != "classif")
    stop("Threshold can only be set for classification predictions!")
  if (pred$predict.type != "prob")
    stop("Threshold can only be set for predict.type 'prob'!")
  levs = td$class.levels
  if (length(levs) == 2L && is.numeric(threshold) && length(threshold) == 1L) {
    threshold = c(threshold, 1-threshold)
    names(threshold) = c(td$positive, td$negative)
  }
  if (length(threshold > 1L) && !setequal(levs, names(threshold)))
    stop("Threshold names must correspond to classes!")
  p = getProbabilities(pred, cl = levs)
  # resort so we have same order in threshold and p
  threshold = threshold[levs]
  #FIXME use BBmisc functuion for max.col here
  pred$data$response = factor(max.col(t(t(p) / threshold)), levels = seq_along(levs), labels = levs)
  pred$threshold = threshold
  return(pred)
}

