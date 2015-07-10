#' @rdname Task
#' @export
makeClusterTask = function(id = deparse(substitute(data)), data, weights = NULL, blocking = NULL, fixup.data = "warn", check.data = TRUE) {
  assertString(id)
  assertDataFrame(data)
  assertChoice(fixup.data, choices = c("no", "quiet", "warn"))
  assertFlag(check.data)

  task = makeUnsupervisedTask("cluster", data, weights, blocking)
  task$task.desc = makeTaskDesc.ClusterTask(task, id)
  addClasses(task, "ClusterTask")
}

makeTaskDesc.ClusterTask = function(task, id) {
  target = character(0L)
  td = makeTaskDescInternal(task, "cluster", id, target)
  return(addClasses(td, c("TaskDescCluster", "TaskDescUnsupervised")))
}

#' @export
print.ClusterTask = function(x, ...) {
  print.UnsupervisedTask(x)
}
