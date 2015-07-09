#' @export
#' @rdname Task
makeRegrTask = function(id = deparse(substitute(data)), data, target, weights = NULL, blocking = NULL, fixup.data = "warn", check.data = TRUE) {
  assertString(id)
  assertDataFrame(data)
  assertString(target)
  assertChoice(fixup.data, choices = c("no", "quiet", "warn"))
  assertFlag(check.data)

  if (fixup.data != "no") {
    if (is.integer(data[[target]]))
      data[[target]] = as.double(data[[target]])
  }

  task = makeSupervisedTask("regr", data, target, weights, blocking, fixup.data = fixup.data, check.data = check.data)

  if (check.data) {
    assertNumeric(data[[target]], any.missing = FALSE, finite = TRUE, .var.name = target)
  }

  task$task.desc = makeTaskDesc.RegrTask(task, id, target)
  addClasses(task, "RegrTask")
}

makeTaskDesc.RegrTask = function(task, id, target) {
  addClasses(makeTaskDescInternal(task, "regr", id, target), c("TaskDescRegr", "TaskDescSupervised"))
}
