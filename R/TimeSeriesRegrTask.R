#' @export
#' @rdname Task
makeTimeSeriesRegrTask = function(id = deparse(substitute(data)), data, target, weights = NULL, blocking = NULL, fixup.data = "warn", check.data = TRUE, channel.list = NULL, formula.list = NULL, index.list = NULL) {
  task = makeSupervisedTask("regr", data, target, weights, blocking, fixup.data = fixup.data, check.data = check.data)
  task = makeRegrTask(id, data, target, weights, blocking, fixup.data, check.data)
  type = "tsregr"
  task$type = type
  task$task.desc$type = type
  task$channel.list = channel.list
  task$formula.list = formula.list
  task$task.desc = addClasses(task$task.desc, "TaskDescTimeSeriesRegr")
  addClasses(task, "TimeSeriesRegrTask")
}
