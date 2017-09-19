#' @title Create a clustering task.
#'
#' @template desc_tasks
#' @templateVar tasktype clustering
#' @templateVar randomtext In clustering tasks, the target variable must not be part of the data set.
#' @templateVar operators :
#'
#' @template arg_id
#' @template arg_data_features_only
#' @template arg_weights
#' @template arg_blocking
#' @template arg_fixup.data
#' @template arg_check.data
#' @return [\code{\link{Task}}].
#' @examples
#'   makeClusterTask(data = iris[, -5L])
#' @export
#' @family task
makeClusterTask = function(id = deparse(substitute(data)), data, weights = NULL, blocking = NULL, fixup.data = "warn", check.data = TRUE) {
  assertString(id)
  assertDataFrame(data)
  assertChoice(fixup.data, choices = c("no", "quiet", "warn"))
  assertFlag(check.data)

  task = makeUnsupervisedTask("cluster", data, weights, blocking, fixup.data, check.data)
  task$task.desc = makeClusterTaskDesc(id, data, weights, blocking)
  addClasses(task, "ClusterTask")
}

makeClusterTaskDesc = function(id, data, weights, blocking) {
  target = character(0L)
  td = makeTaskDescInternal("cluster", id, data, target, weights, blocking)
  return(addClasses(td, c("ClusterTaskDesc", "UnsupervisedTaskDesc")))
}

#' @export
print.ClusterTask = function(x, ...) {
  print.UnsupervisedTask(x)
}
