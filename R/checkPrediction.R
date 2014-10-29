checkPrediction = function(p, task.type = NULL, predict.type = NULL) {
  if (!is.null(task.type) && p$task.desc$type %nin% task.type)
    stopf("Prediction must be one of '%s', but is: '%s'", collapse(task.type), p$task.desc$type)
  if (!is.null(predict.type) && p$predict.type %nin% predict.type)
    stopf("predict.type must be one of '%s', but is: '%s'", collapse(predict.type), p$predict.type)
}
