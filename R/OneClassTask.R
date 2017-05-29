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
  
  task$task.desc = makeTaskDesc.OneClassTask(task, id)
  addClasses(task, "OneClassTask") 
  
}

makeTaskDesc.OneClassTask = function(task, id) {
  target = character(0L)
  td = makeTaskDescInternal(task, "oneclass", id, target)
  return(addClasses(td, c("TaskDescOneClass", "TaskDescUnsupervised")))
}

#' @export
print.OneClassTask = function(x, ...) {
  print.UnsupervisedTask(x)
}
