#' @title Create a clustering task.
#'
#' @template desc_tasks
#' @templateVar desc_tasks_tasktype clustering
#' @templateVar desc_tasks_further_notes In clustering tasks, the target variable must not be part of the data set.
#'
#' @inheritParams makeTask
#' @template arg_id
#' @template arg_data_features_only
#' @return [\code{\link{ClusterTask}}].
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
