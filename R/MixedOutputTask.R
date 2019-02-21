#' @export
#' @rdname Task
makeMultioutputTask = function(id = deparse(substitute(data)), data, target, weights = NULL,
  blocking = NULL, coordinates = NULL, fixup.data = "warn", check.data = TRUE) {
  assertString(id)
  assertCharacter(target, any.missing = FALSE, min.len = 2L)
  assertDataFrame(data)
  assertChoice(fixup.data, choices = c("no", "quiet", "warn"))
  assertFlag(check.data)

  task = makeSupervisedTask("multioutput", data = data, target = target, weights = weights, blocking = blocking,
        coordinates = coordinates, fixup.data = fixup.data, check.data = check.data)
  task$task.desc = makeMultioutputTaskDesc(id, data, target, weights, blocking, coordinates)
  task$target.type = sapply(getTaskTargets(task), class)
  task = addClasses(task, c("MultioutputTask", "MultiOutputTask"))
  return(task)
}

#' @export
print.MultioutputTask = function(x, ...) {
  y = getTaskTargets(x)
  print.SupervisedTask(x)
  catf("Classes: %i", ncol(y))
  print(sapply(y, class))
}

#' @export
#' @rdname makeTaskDesc
makeMultioutputTaskDesc = function(id, data, target, weights, blocking, coordinates) {
  levs = target
  td = makeTaskDescInternal("multioutput", id, data, target, weights, blocking, coordinates)
  td$class.levels = levs
  return(addClasses(td, c("MultioutputTaskDesc", "SupervisedTaskDesc")))
}
