checkPrediction = function(pred, task.type = NULL, predict.type = NULL, check.truth = FALSE) {
  assertClass(p, "Prediction")
  if (!is.null(task.type) && pred$task.desc$type %nin% task.type)
    stopf("Prediction must be one of '%s', but is: '%s'", collapse(task.type), pred$task.desc$type)
  if (!is.null(predict.type) && pred$predict.type %nin% predict.type)
    stopf("predict.type must be one of '%s', but is: '%s'", collapse(predict.type), pred$predict.type)
  if (check.truth && is.null(pred$data$truth))
    stopf("Prediction object does not contain ground truth column 'truth'!")
}
