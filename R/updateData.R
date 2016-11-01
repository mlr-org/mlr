#' @title Update Task Data
#'
#' @description
#' Update a task with new data.
#'
#' @param task [\code{\link{Task}}]\cr
#'   The task.
#' @param newdata [\code{data.frame}]\cr
#'   New observations to update the model
#' @param weights [\code{numeric}]\cr
#'   Optional, non-negative case weight vector to be used during fitting.
#'   If given, must be of same length as \code{subset} and in corresponding order.
#'   By default \code{NULL} which means no weights are used unless specified in the task (\code{\link{Task}}).
#'   Weights from the task will be overwritten.
#' @param ... [any]\cr
#'   Currently ignored.
#' @return [\code{\link{WrappedModel}}].
#' @export
updateData = function(task, newdata, weights,...) {
  assertClass(task, "Task")
  if (missing(weights))
    weights = task$weights
  if (missing(newdata))
    stop("New data must be supplied")
  if (xts::is.xts(newdata))
    newdata = as.data.frame(dates = index(newdata), newdata)
  assertDataFrame(newdata)
  data = getTaskData(task)
  data = rbind(data,newdata)
  changeData(task, data, weights)
}
