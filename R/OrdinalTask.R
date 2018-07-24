#' @export
#' @rdname Task
makeOrdinalTask = function(id = deparse(substitute(data)), data, target, weights = NULL, blocking = NULL, coordinates = NULL, fixup.data = "warn", check.data = TRUE) {
  assertString(id)
  assertDataFrame(data)
  assertString(target)
  assertChoice(fixup.data, choices = c("no", "quiet", "warn"))
  assertFlag(check.data)

  if (fixup.data != "no") {
    if (is.integer(data[[target]]))
      data[[target]] = as.double(data[[target]])
  }

  task = makeSupervisedTask("ordinal", data, target, weights, blocking, coordinates, fixup.data = fixup.data, check.data = check.data)

  if (check.data) {
    assertFactor(data[[target]], any.missing = FALSE, ordered = TRUE, empty.levels.ok = FALSE, .var.name = target)
  }

  task$task.desc = makeOrdinalTaskDesc(id, data, target, weights, blocking, coordinates)
  addClasses(task, "OrdinalTask")
}

#' @export
print.OrdinalTask = function(x, ...) {
  y = levels(getTaskTargets(x))
  print.SupervisedTask(x)
  cat("Levels:", paste(y, collapse = " < "))
}

#' @export
#' @rdname makeTaskDesc
makeOrdinalTaskDesc = function(id, data, target, weights, blocking, coordinates) {
  levs = levels(data[[target]])
  td = makeTaskDescInternal("ordinal", id, data, target, weights, blocking, coordinates)
  td$ordinal.levels = levs
  td$level.distribution = table(data[target])
  addClasses(td, c("OrdinalTaskDesc", "SupervisedTaskDesc"))
}
