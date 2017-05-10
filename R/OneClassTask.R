#' @export
#' @rdname Task

makeOneClassTask = function(id = deparse(substitute(data)), data, target,
  weights = NULL, blocking = NULL, positive = NA_character_, fixup.data = "warn", check.data = TRUE) {
  assertString(id)
  assertDataFrame(data)
  if (!missing(target)) {
    assertString(target)
  } else {
    data$anomaly = FALSE
    target = "anomaly"
    messagef("No target column specified, add target column 'anomaly' with one class 'FALSE'(= 'normal')")
  }
  # some code on cran passed stuff like positive=1, we can live with the convert here
  if (isScalarNumeric(positive))
    positive = as.character(positive)
  assertString(positive, na.ok = TRUE)
  assertChoice(fixup.data, choices = c("no", "quiet", "warn"))
  assertFlag(check.data)

  if (fixup.data != "no") {
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

  if (check.data) {
    assertFactor(data[[target]], any.missing = FALSE, empty.levels.ok = FALSE, .var.name = target)
    if (length(levels(data[[target]])) > 2)
      stopf("Target column '%s' contains more than two factor levels")
  }

  task$task.desc = makeOneClassTaskDesc(id, data, target, weights, blocking, positive)
  addClasses(task, "OneClassTask")
}

makeOneClassTaskDesc = function(id, data, target, weights, blocking, positive) {
  levs = levels(data[[target]])
  m = length(levs)
  if (is.na(positive)) {
    if (m <= 2L) {
      positive = setdiff(c(TRUE,FALSE), names(which.max(table(data[[target]]))))
    }
  } else {
    if (m > 2L)
      stop("Cannot set a positive class for a multiclass problem!")
    assertChoice(positive, choices = levs)
  }
  td = makeTaskDescInternal("oneclass", id, data, target, weights, blocking)
  td$class.levels = levs
  td$positive = positive
  td$negative = NA_character_
  if (length(td$class.levels) == 1L)
    td$negative = setdiff(c(TRUE, FALSE), positive)
  else if (length(td$class.levels) == 2L)
    td$negative = setdiff(td$class.levels, positive)
  return(addClasses(td, c("OneClassTaskDesc", "SupervisedTaskDesc")))
}

#' @export
print.OneClassTask = function(x, ...) {
  di = printToChar(table(getTaskTargets(x)), collapse = NULL)[-1L]
  m = length(x$task.desc$class.levels)
  print.SupervisedTask(x)
  catf("Classes: %i", m)
  catf(collapse(di, "\n"))
  catf("Negative/Normal class: %s", x$task.desc$negative)
  catf("Positive/Anomaly class: %s", x$task.desc$positive)
  catf("Target column \"anomaly\" is not used in training, \n but is needed in case of evaluation with classification measures.")
}

