#' @rdname Task
#' @export
makeClusterTask = function(id, data, weights = NULL, blocking = NULL,
  fixup.data = "warn", check.data = TRUE) {
  assertChoice(fixup.data, choices = c("no", "quiet", "warn"))
  assertFlag(check.data)

  task = addClasses(makeUnsupervisedTask("cluster", data, weights, blocking), "ClusterTask")
  if (fixup.data != "no")
    fixupData(task, character(0L), fixup.data)
  if (check.data)
    checkTaskCreation(task, target = character(0L))
  id = checkOrGuessId(id, data)
  task$task.desc = makeTaskDesc.ClusterTask(task, id)
  return(task)
}

checkTaskCreation.ClusterTask = function(task, target, ...) {
  NextMethod("checkTaskCreation")
}

fixupData.ClusterTask = function(task, target, choice, ...) {
  NextMethod("fixupData")
}

makeTaskDesc.ClusterTask = function(task, id) {
  target = character(0L)
  td = makeTaskDescInternal(task, "cluster", id, target)
  return(addClasses(td, "TaskDescCluster"))
}

#' @export
print.ClusterTask = function(x, ...) {
  print.UnsupervisedTask(x)
}
