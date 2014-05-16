#' @export
#' @rdname SupervisedTask
makeRegrTask = function(id, data, target, weights = NULL, blocking = NULL,
  fixup.data = "warn", check.data = TRUE) {
  checkArg(fixup.data, choices = c("no", "quiet", "warn"))
  checkArg(check.data, "logical", len = 1L, na.ok = FALSE)

  task = addClasses(makeSupervisedTask("regr", data, target, weights, blocking), "RegrTask")
  if (fixup.data != "no")
    fixupData(task, target, fixup.data)
  if (check.data)
    checkTask(task, target)

  id = checkOrGuessId(id, data)
  task$task.desc = makeTaskDesc.RegrTask(task, id, target)
  return(task)
}

#' @export
checkTask.RegrTask = function(task, target, ...) {
  NextMethod("checkTask")
  checkArg(target, "character", len = 1L)
  if (!is.numeric(task$env$data[[target]]))
    stopf("Target column '%s' must be numeric", target)
}

#' @export
fixupData.RegrTask = function(task, target, choice, ...) {
  NextMethod("fixupData")
  x = task$env$data[[target]]
  if (is.integer(x))
    task$env$data[[target]] = as.numeric(x)
}

#' @export
makeTaskDesc.RegrTask = function(task, id, target) {
  addClasses(makeTaskDescInternal(task, "regr", id, target), "TaskDescRegr")
}
