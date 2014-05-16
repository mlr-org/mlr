#' Converts predictions to a format package ROCR can handle.
#'
#' @param pred [\code{\link{Prediction}} | \code{\link{Prediction}}] \cr
#'		Prediction object.
#' @export
asROCRPrediction = function(pred) {
  UseMethod("asROCRPrediction")
}

#' @export
asROCRPrediction.Prediction = function(pred) {
  if(length(pred$task.desc$class.levels) != 2L) {
    stop("More than 2 classes!")
  }
  p = getProbabilities(pred)
  ROCR::prediction(p, pred$data$truth, label.ordering=c(pred$task.desc$negative, pred$task.desc$positive))
}

#' @export
asROCRPrediction.ResamplePrediction = function(pred) {
  if(length(pred$task.desc$class.levels) != 2L) {
    stop("More than 2 classes!")
  }
  prob = getProbabilities(pred)
  iter = pred$data$iter
  prob = split(prob, iter)
  truth = split(pred$data$truth, iter)
  ROCR::prediction(prob, truth, label.ordering=c(pred$task.desc$negative, pred$task.desc$positive))
}
