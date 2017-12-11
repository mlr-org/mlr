#' @rdname Task
#' @export
makeClusterTask = function(id = deparse(substitute(data)), data, weights = NULL, blocking = NULL, spatial = FALSE, fixup.data = "warn", check.data = TRUE) {
  assertString(id)
  assertDataFrame(data)
  assertChoice(fixup.data, choices = c("no", "quiet", "warn"))
  assertFlag(check.data)

  task = makeUnsupervisedTask("cluster", data = data, weights = weights,
                              blocking = blocking, fixup.data = fixup.data,
                              check.data = check.data, spatial = spatial)
  task$task.desc = makeClusterTaskDesc(id, data, weights, blocking, spatial)
  addClasses(task, "ClusterTask")
}

makeClusterTaskDesc = function(id, data, weights, blocking, spatial) {
  target = character(0L)
  td = makeTaskDescInternal("cluster", id, data, target, weights, blocking, spatial)
  return(addClasses(td, c("ClusterTaskDesc", "UnsupervisedTaskDesc")))
}

#' @export
print.ClusterTask = function(x, ...) {
  print.UnsupervisedTask(x)
}
