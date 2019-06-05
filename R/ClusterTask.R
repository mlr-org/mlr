#' @title Create a cluster task.
#' @inheritParams Task
#' @seealso [Task] [ClassifTask] [CostSensTask] [MultilabelTask] [RegrTask] [SurvTask]
#' @rdname ClusterTask
#' @aliases ClusterTask
#' @export
makeClusterTask = function(id = deparse(substitute(data)), data, weights = NULL, blocking = NULL, coordinates = NULL, fixup.data = "warn", check.data = TRUE) {
  assertString(id)
  assertDataFrame(data)
  assertChoice(fixup.data, choices = c("no", "quiet", "warn"))
  assertFlag(check.data)

  task = makeUnsupervisedTask("cluster", data = data, weights = weights,
    blocking = blocking, fixup.data = fixup.data,
    check.data = check.data, coordinates = coordinates)
  task$task.desc = makeClusterTaskDesc(id, data, weights, blocking, coordinates)
  addClasses(task, "ClusterTask")
}

#' @export
#' @rdname makeTaskDesc
makeClusterTaskDesc = function(id, data, weights, blocking, coordinates) {
  target = character(0L)
  td = makeTaskDescInternal("cluster", id, data, target, weights, blocking, coordinates)
  return(addClasses(td, c("ClusterTaskDesc", "UnsupervisedTaskDesc")))
}

#' @export
print.ClusterTask = function(x, ...) {
  print.UnsupervisedTask(x)
}
