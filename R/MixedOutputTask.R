#' @export
#' @rdname Task
makeMixedOutputTask = function(id = deparse(substitute(data)), data, target, weights = NULL,
  blocking = NULL, coordinates = NULL, fixup.data = "warn", check.data = TRUE) {
  assertString(id)
  assertCharacter(target, any.missing = FALSE, min.len = 2L)
  assertDataFrame(data)
  assertChoice(fixup.data, choices = c("no", "quiet", "warn"))
  assertFlag(check.data)

  task = makeSupervisedTask("mixedoutput", data = data, target = target, weights = weights, blocking = blocking,
        coordinates = coordinates, fixup.data = fixup.data, check.data = check.data)
  task$task.desc = makeMixedOutputTaskDesc(id, data, target, weights, blocking, coordinates)
  task$target.type = sapply(getTaskTargets(task), class)
  task = addClasses(task, c("MixedOutputClass", "MultiOutputClass"))
  return(task)
}

#' @export
print.MixedOutputClass = function(x, ...) {
  y = getTaskTargets(x)
  print.SupervisedTask(x)
  catf("Classes: %i", ncol(y))
  print(sapply(y, class))
}

#' @export
#' @rdname makeTaskDesc
makeMixedOutputTaskDesc = function(id, data, target, weights, blocking, coordinates) {
  levs = target
  td = makeTaskDescInternal("mixedoutput", id, data, target, weights, blocking, coordinates)
  td$class.levels = levs
  return(addClasses(td, c("MixedOutputTaskDesc", "SupervisedTaskDesc")))
}
