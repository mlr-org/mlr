checkPrediction = function(pred, task.type = NULL, binary = FALSE, predict.type = NULL, check.truth = FALSE, no.na = TRUE) {
  assertClass(pred, "Prediction")
  if (!is.null(task.type) && pred$task.desc$type %nin% task.type) {
    stopf("Prediction must be one of '%s', but is: '%s'", collapse(task.type), pred$task.desc$type)
  }
  if (binary) {
    nlevs = length(pred$task.desc$class.levels)
    if (nlevs != 2L) {
      stopf("Prediction must be for binary classification, but has %i class levels!", nlevs)
    }
  }
  if (!is.null(predict.type) && pred$predict.type %nin% predict.type) {
    stopf("predict.type must be one of '%s', but is: '%s'", collapse(predict.type), pred$predict.type)
  }
  if (check.truth && is.null(pred$data$truth)) {
    stopf("Prediction object does not contain ground truth column 'truth'!")
  }

  if (no.na) {
    r = getPredictionResponse(pred)
    if (anyMissing(r)) {
      stopf("Prediction object contains NAs in response, this likely due to a prediction from a FailureModel!")
    }
  }
}
