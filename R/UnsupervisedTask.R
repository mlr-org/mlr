makeUnsupervisedTask = function(type, data, weights = NULL, blocking = NULL, fixup.data = "warn", check.data = TRUE) {
  task = makeTask(type, data, weights, blocking, fixup.data = fixup.data, check.data = check.data)
  checkTaskData(getTaskData(data))
  addClasses(task, "UnsupervisedTask")
}

#' @export
print.UnsupervisedTask = function(x, print.weights = TRUE, ...) {
  td = x$task.desc
  catf("Unsupervised task: %s", td$id)
  catf("Type: %s", td$type)
  catf("Observations: %i", td$size)
  catf("Features:")
  catf(printToChar(td$n.feat, collapse = "\n"))
  catf("Missings: %s", td$has.missings)
  if (print.weights)
    catf("Has weights: %s", td$has.weights)
  catf("Has blocking: %s", td$has.blocking)
}
