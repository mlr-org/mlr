#' @export
estimateResidualVariance = function(x, task, data, target) {
  UseMethod("estimateResidualVariance")
}

#' @export
estimateResidualVariance.Learner = function(x, task, data, target) {
  if (missing(task)) {
    checkArg(data, "data.frame")
    checkArg(target, "character", len = 1L, na.ok = FALSE)
    task = makeRegrTask(data = data, target = target)
  } else {
    checkArg(task, "RegrTask")
  }
  model = train(x, task)
  estimateResidualVariance.WrappedModel(model, task = task)
}

#' @export
estimateResidualVariance.WrappedModel = function(x, task, data, target) {
  if (missing(task)) {
    checkArg(data, "data.frame")
    checkArg(target, "character", len = 1L, na.ok = FALSE)
    task = makeRegrTask(data = data, target = target)
  } else {
    checkArg(task, "RegrTask")
  }
  p = predict(x, task)
  var(p$data$response - p$data$truth)
}







