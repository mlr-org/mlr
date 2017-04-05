#' @export
#' @rdname Task
makeRegrTask = function(id = deparse(substitute(data)), data, target, weights = NULL,
  blocking = NULL, fixup.data = "warn", check.data = TRUE, formula = NULL) {
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

  task$task.desc = makeRegrTaskDesc(id, data, target, weights, blocking, formula = formula)  addClasses(task, "RegrTask")
}

makeRegrTaskDesc = function(id, data, target, weights, blocking, formula = NULL) {
  addClasses(makeTaskDescInternal("regr", id, data, target, weights, blocking, formula), c("RegrTaskDesc", "SupervisedTaskDesc"))}
