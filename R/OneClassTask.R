#' @export
#' @rdname Task

makeOneClassTask = function(id = deparse(substitute(data)), data,
  weights = NULL, blocking = NULL, fixup.data = "warn", check.data = TRUE) {
  assertString(id)
  assertDataFrame(data)
  # some code on cran passed stuff like positive=1, we can live with the convert here
  # for one class positive is always set to the one class
  assertChoice(fixup.data, choices = c("no", "quiet", "warn"))
  assertFlag(check.data)

  task = makeUnsupervisedTask("oneclass", data, weights, blocking,
    fixup.data = fixup.data, check.data = check.data)

  task$task.desc = makeOneClassTaskDesc(id, data, weights, blocking)
  addClasses(task, "OneClassTask")

}

makeOneClassTaskDesc = function(id, data, weights, blocking) {
  target = character(0L)
  td = makeTaskDescInternal("oneclass", id, data, target, weights, blocking)
  return(addClasses(td, c("OneClassTaskDesc", "UnsupervisedTaskDesc")))
}

#' @export
print.OneClassTask = function(x, ...) {
  print.UnsupervisedTask(x)
}
