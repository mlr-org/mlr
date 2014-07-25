#' @export
#' @rdname Task
makeRegrTask = function(id, data, target, weights = NULL, blocking = NULL,
  fixup.data = "warn", check.data = TRUE) {
  assertChoice(fixup.data, choices = c("no", "quiet", "warn"))
  assertFlag(check.data)

  task = addClasses(makeSupervisedTask("regr", data, target, weights, blocking), "RegrTask")
  if (fixup.data != "no")
    fixupData(task, target, fixup.data)
  if (check.data)
    checkTaskCreation(task, target)

  id = checkOrGuessId(id, data)
  task$task.desc = makeTaskDesc.RegrTask(task, id, target)
  return(task)
}

checkTaskCreation.RegrTask = function(task, target, ...) {
  NextMethod("checkTaskCreation")
  assertString(target)
  if (!is.numeric(task$env$data[[target]]))
    stopf("Target column '%s' must be numeric", target)
}

fixupData.RegrTask = function(task, target, choice, ...) {
  NextMethod("fixupData")
  x = task$env$data[[target]]
  if (is.integer(x))
    task$env$data[[target]] = as.numeric(x)
}

makeTaskDesc.RegrTask = function(task, id, target) {
  addClasses(makeTaskDescInternal(task, "regr", id, target), "TaskDescRegr")
}
