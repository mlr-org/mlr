#' Estimate the residual variance.
#'
#' Estimate the residual variance of a regression model on a given task.
#' If a regression learner is provided instead of a model, the model is
#' trained (see [train]) first.
#'
#' @param x ([Learner] or [WrappedModel])\cr
#'   Learner or wrapped model.
#' @param task ([RegrTask])\cr
#'   Regression task.
#'   If missing, `data` and `target` must be supplied.
#' @param data ([data.frame])\cr
#'   A data frame containing the features and target variable.
#'   If missing, `task` must be supplied.
#' @param target (`character(1)`)\cr
#'   Name of the target variable.
#'   If missing, `task` must be supplied.
#' @export
estimateResidualVariance = function(x, task, data, target) {
  UseMethod("estimateResidualVariance")
}

#' @export
estimateResidualVariance.Learner = function(x, task, data, target) {
  if (missing(task)) {
    task = makeRegrTask(data = data, target = target)
  }
  estimateResidualVariance.WrappedModel(train(x, task), task)
}

#' @export
estimateResidualVariance.WrappedModel = function(x, task, data, target) {
  if (missing(task)) {
    task = makeRegrTask(data = data, target = target)
  } else {
    assertClass(task, classes = "RegrTask")
  }
  p = predict(x, task)
  var(p$data$response - p$data$truth)
}
