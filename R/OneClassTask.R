#' @export
#' @rdname Task

makeOneClassTask = function(id = deparse(substitute(data)), data, target = NULL,
  weights = NULL, blocking = NULL, fixup.data = "warn", positive, negative, check.data = TRUE) {
  assertString(id)

  # positive needs to be a string, if it's a number convert it into string
  assert(
    checkString(positive),
    checkNumber(positive)
  )
  if (isScalarNumeric(positive))
    positive = as.character(positive)

  assert(
    checkString(negative),
    checkNumber(negative)
  )
  if (isScalarNumeric(negative))
    negative = as.character(negative)

  assertDataFrame(data)
  if (!is.null(target))
    assertString(target) # that this is a valid colname will be check later in makeSupervisedTask

  assertChoice(fixup.data, choices = c("no", "quiet", "warn"))
  assertFlag(check.data)

  if (fixup.data != "no") {
    if (is.null(target)) {
      if ("normal" %in% colnames(data))
        stopf("Trying to auto-create one-class target column named 'normal', but dataset already has a sucha column.
          Please check and possibly rename. Or set argument 'target' manually.")
      data$normal = factor(rep(positive, nrow(data)), levels = c(positive, negative))
      target = "normal"
      messagef("No target column specified, auto-creating target column 'normal' with only normal elements.")
    }
    x = data[[target]]
    # we need the is.factor check here to allow the (safe) adding of levels in the if-body
    if (is.character(x) || is.logical(x) || is.integer(x) || is.factor(x)) {
      data[[target]] = as.factor(x)
      levs = levels(data[[target]])
      # add pos and neg as levels if they are missing
      if (positive %nin% levs) levels(data[[target]]) = c(levs, positive)
      if (negative %nin% levs) levels(data[[target]]) = c(levs, positive)
      # after this line we have either something weird or a factor with 2 levels: pos and neg
    }
    # we probably dont want to autodrop empty target levels here (as in classif), as the anomaly class could be empy
  }

  task = makeSupervisedTask("oneclass", data, target, weights, blocking,
    fixup.data = fixup.data, check.data = check.data)

  # check that class column is factor and has 2 class levels
  if (check.data) {
    assertFactor(data[[target]], any.missing = FALSE, levels = c(positive, negative),
      empty.levels.ok = TRUE, .var.name = target)
  }
  task$task.desc = makeOneClassTaskDesc(id, data, target, weights, blocking, positive, negative)
  addClasses(task, "OneClassTask")
}

makeOneClassTaskDesc = function(id, data, target, weights, blocking, positive, negative) {
  td = makeTaskDescInternal("oneclass", id, data, target, weights, blocking)
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
  catf("Positive/Normal class: %s", x$task.desc$positive)
  catf("Negative/Anomaly class: %s", x$task.desc$negative)
}

