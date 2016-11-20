#' @export
#' @rdname Task
makeTimeSeriesClassifTask = function(id = deparse(substitute(data)), data, target, weights = NULL, blocking = NULL, positive = NA_character_, fixup.data = "warn", check.data = TRUE) {
  task = makeSupervisedTask("classif", data, target, weights, blocking, fixup.data = fixup.data, check.data = check.data)

  task = makeClassifTask(id, data, target, weights, blocking, positive, fixup.data, check.data)
  type = "tsclassif"
  task$type = type
  task$task.desc$type = type
  task$task.desc = addClasses(task$task.desc, "TaskDescTimeSeriesClassif")
  addClasses(task, "TimeSeriesClassifTask")
}


