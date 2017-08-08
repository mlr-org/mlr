#' @export
#' @rdname Task

makeOneClassTask = function(id = deparse(substitute(data)), data, target,
  weights = NULL, blocking = NULL, fixup.data = "warn", positive, negative,
  check.data = TRUE) {
  assertString(id)

  # positive needs to be a string, if it's a number convert it into string
  assert(
    checkString(positive),
    checkNumber(positive)
  )
  if (isScalarNumeric(positive))
    positive = as.character(positive)

  # negative needs to be a string, if it's a number convert it into string
  assert(
    checkString(negative),
    checkNumber(negative)
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

  # check if positive and negative are element of class levels
  levs = levels(data[[target]])

  if (length(levs) == 2) {
    if (!setequal(c(positive, negative), levs))
      stopf("'positive' or 'negative' not equal to class levels")
  } else if (length(levs) == 1) {
    if (sum(c(positive, negative) %in% levs) == 0)
      stopf("Neither 'positive' nor 'negative' are subset of class levels")
  }

  task = makeSupervisedTask("oneclass", data, target, weights, blocking,
    fixup.data = fixup.data, check.data = check.data)


  if (fixup.data != "no") {
    if (length(levs) == 1)
      levels(task$env$data[[target]]) = union(levs, c(positive, negative))
  }

  task$task.desc = makeOneClassTaskDesc(id, data, target, weights, blocking, positive, negative)
  addClasses(task, "OneClassTask")
}

makeOneClassTaskDesc = function(id, data, target, weights, blocking, positive, negative) {
  td = makeTaskDescInternal("oneclass", id, data, target, weights, blocking)
  levs = levels(data[[target]])
  td$class.levels = c(positive, negative)
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
  catf("Positive/Anomaly class: %s", x$task.desc$positive)
  catf("Negative/Normal class: %s", x$task.desc$negative)
}

