#' @export
#' @rdname Task

makeOneClassTask = function(id = deparse(substitute(data)), data, target,
  weights = NULL, blocking = NULL, fixup.data = "warn", positive = NA_character_, negative = NA_character_,
    check.data = TRUE) {
  assertString(id)

  # positive needs to be a string, if it's a number convert it into string
  assert(
    checkString(positive, na.ok = TRUE),
    checkNumber(positive, na.ok = TRUE)
  )
  if (isScalarNumeric(positive))
    positive = as.character(positive)

  assert(
    checkString(negative, na.ok = TRUE),
    checkNumber(negative, na.ok = TRUE)
  )
  if (isScalarNumeric(negative))
    negative = as.character(negative)

  assertDataFrame(data)
  assertString(target) # that this is a valid colname will be check later in makeSupervisedTask

  assertChoice(fixup.data, choices = c("no", "quiet", "warn"))
  assertFlag(check.data)

  if (fixup.data != "no") {
    x = data[[target]]
    if (is.character(x) || is.logical(x) || is.integer(x)) {
      data[[target]] = as.factor(x)
    }
    # we probably dont want to autodrop empty target levels here (as in classif), as the anomaly class could be empty
  }
  # check that class column is factor and has max 2 class levels
  if (check.data) {
    assertFactor(data[[target]], any.missing = FALSE, empty.levels.ok = TRUE, max.levels = 2L, .var.name = target)
  }

  task = makeSupervisedTask("oneclass", data, target, weights, blocking,
    fixup.data = fixup.data, check.data = check.data)

  if (fixup.data != "no") {
    levs = levels(data[[target]])
    # add pos and neg as levels if they are missing
    if (length(levs) == 1) {
      if (positive %nin% levs) levels(data[[target]]) = c(levs, positive)
      if (negative %nin% levs) levels(data[[target]]) = c(levs, negative)
    }
    task$env$data = data
  }

  task$task.desc = makeOneClassTaskDesc(id, data, target, weights, blocking, positive, negative)
  addClasses(task, "OneClassTask")
}

makeOneClassTaskDesc = function(id, data, target, weights, blocking, positive, negative) {
  td = makeTaskDescInternal("oneclass", id, data, target, weights, blocking)
  levs = levels(data[[target]])
  m = length(levs)
  if (is.na(positive)) {
    positive = levs[1L]
  }
  if (is.na(negative)) {
    if (m < 2L)
      stopf("Cannot auto-set negative class when there are < 2 class levels!")
    negative = levs[2L]
  }
  posneg = c(positive, negative)
  assertSetEqual(levs, posneg)
  td$class.levels = posneg
  td$positive = positive
  td$negative = negative
  return(addClasses(td, c("OneClassTaskDesc", "SupervisedTaskDesc")))
}

#' @export
print.OneClassTask = function(x, ...) {
  di = printToChar(table(getTaskTargets(x)), collapse = NULL)[-1L]
  m = length(x$task.desc$class.levels)
  print.SupervisedTask(x)
  catf("Classes: %i", m)
  catf(collapse(di, "\n"))
  catf("Positive/Normal class: %s", x$task.desc$positive)
  catf("Negative/Anomaly class: %s", x$task.desc$negative)
}

