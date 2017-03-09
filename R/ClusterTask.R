#' @rdname Task
#' @export
makeClusterTask = function(id = deparse(substitute(data)), data, weights = NULL, blocking = NULL, fixup.data = "warn", check.data = TRUE) {
  assertString(id)
  assertDataFrame(data)
  assertChoice(fixup.data, choices = c("no", "quiet", "warn"))
  assertFlag(check.data)

  task = makeUnsupervisedTask("cluster", data, weights, blocking)
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
