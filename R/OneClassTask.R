#' @export
#' @rdname Task

makeOneClassTask = function(id = deparse(substitute(data)), data, target = NULL,
  weights = NULL, blocking = NULL, fixup.data = "warn", positive, negative, check.data = TRUE) {
  assertString(id)

  # positive needs to be a string, if it's a number convert it into string
  # makeSupervisedTask will check if it's a level of target
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
      if ("normal" %in% names(data)) {
        stopf("Given dataset already has a column named 'normal', can't create a target column with the same name. Please rename the 'normal' column.")
      }
      data$normal = positive
      target = "normal"
      messagef("No target column specified, add target column 'normal' with one class 'TRUE' as the positive class \n
        As the assumption for oneclass classification is that one only have observation of one class.")
    }
    x = data[[target]]
    if (is.character(x) || is.logical(x) || is.integer(x)) {
      data[[target]] = as.factor(x)
    } else if (is.factor(x) && fixup.data == "warn" && hasEmptyLevels(x)) {
      warningf("Target column '%s' contains empty factor levels", target)
      data[[target]] = droplevels(x)
    }
  }

  task = makeSupervisedTask("oneclass", data, target, weights, blocking,
    fixup.data = fixup.data, check.data = check.data)

  # check that class column is factor and has <=2 class levels
  if (check.data) {
    assertFactor(data[[target]], any.missing = FALSE, empty.levels.ok = FALSE, .var.name = target)
    if (length(levels(data[[target]])) > 2)
      stopf("Target column '%s' contains more than two factor levels")
  }
  # #FIXME: böse
  levs = levels(data[[target]])
    if (sum(levs %in% c(positive, negative)) >= 1) {
      if (length(levs) == 1)
        levels(data[[target]]) = union(levs, c(positive, negative))
    } else {
      stopf("Levels of target column is not element of defined positive or negative class.")
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
  #FIXME: gehört in die doku
  catf("Note: As oneclass classification problem is an unsupervised learning problem,
    the label TRUE and FALSE aren't the ground truth, if the class column is automatecially created by mlR,
    but rather an assumption of the oneclass classification problem.")
}

