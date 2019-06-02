#' Converts predictions to a format package ROCR can handle.
#'
#' @template arg_pred
#' @export
#' @family roc
#' @family predict
asROCRPrediction = function(pred) {
  UseMethod("asROCRPrediction")
}

asROCRPredictionIntern = function(probabilites, truth, negative, positive) {
  ROCR::prediction(probabilites, truth, label.ordering = c(negative, positive))
}

#' @export
asROCRPrediction.Prediction = function(pred) {
  if (length(pred$task.desc$class.levels) != 2L) {
    stop("More than 2 classes!")
  }
  asROCRPredictionIntern(getPredictionProbabilities(pred), pred$data$truth, pred$task.desc$negative, pred$task.desc$positive)
}

#' @export
asROCRPrediction.ResamplePrediction = function(pred) {
  if (length(pred$task.desc$class.levels) != 2L) {
    stop("More than 2 classes!")
  }
  prob = getPredictionProbabilities(pred)
  iter = pred$data$iter
  prob = split(prob, iter)
  truth = split(pred$data$truth, iter)
  ROCR::prediction(prob, truth, label.ordering = c(pred$task.desc$negative, pred$task.desc$positive))
}
