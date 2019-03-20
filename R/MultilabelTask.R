#' @export
#' @rdname Task
makeMultilabelTask = function(id = deparse(substitute(data)), data, target, weights = NULL,
  blocking = NULL, coordinates = NULL, fixup.data = "warn", check.data = TRUE) {
  assertString(id)
  assertCharacter(target, any.missing = FALSE, min.len = 2L)
  assertDataFrame(data)
  assertChoice(fixup.data, choices = c("no", "quiet", "warn"))
  assertFlag(check.data)

  task = makeSupervisedTask("multilabel", data = data, target = target,
                            weights = weights, blocking = blocking,
                            coordinates = coordinates, fixup.data = fixup.data,
                            check.data = check.data)
  # currently we dont do any fixup here
  if (check.data) {
    for (cn in target)
      assertLogical(task$env$data[[cn]], any.missing = FALSE, .var.name = cn)
  }
  task$task.desc = makeMultilabelTaskDesc(id, data, target, weights, blocking, coordinates)
  addClasses(task, "MultilabelTask")
}

#' @export
print.MultilabelTask = function(x, ...) {
  y = getTaskTargets(x)
  sums = colSums(y)
  print.SupervisedTask(x)
  catf("Classes: %i", ncol(y))
  print(sums)
}

#' @export
#' @rdname makeTaskDesc
makeMultilabelTaskDesc = function(id, data, target, weights, blocking, coordinates) {
  levs = target
  td = makeTaskDescInternal("multilabel", id, data, target, weights, blocking, coordinates)
  td$class.levels = levs
  return(addClasses(td, c("MultilabelTaskDesc", "SupervisedTaskDesc")))
}
