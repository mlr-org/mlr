makeUnsupervisedTask = function(type, data, weights, blocking, fixup.data, check.data, coordinates) {
  task = makeTask(type, data, weights, blocking, fixup.data = fixup.data, check.data = check.data,
    coordinates = coordinates)
  if (check.data) {
    # we can't use getTaskData to access the tasks's data here because we then
    # want to access the description object which is not existing yet
    checkTaskData(task$env$data)
  }
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
  if (print.weights) {
    catf("Has weights: %s", td$has.weights)
  }
  catf("Has blocking: %s", td$has.blocking)
  catf("Has coordinates: %s", td$has.coordinates)
}
