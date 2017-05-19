#' @export
#' @rdname Task

makeOneClassTask = function(id = deparse(substitute(data)), data, target,
  weights = NULL, blocking = NULL, positive = NA_character_, fixup.data = "warn", check.data = TRUE) {
  assertString(id)
  assertDataFrame(data)
  if (!missing(target)) {
    assertString(target)
  } else {
    data$normal = TRUE
    target = "normal"
    messagef("No target column specified, add target column 'normal' with one class 'TRUE' \n
      As the assumption for oneclass classification is that one only have observation of one class.")
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
  levels(data[[target]]) = union(levels(data[[target]]), c(TRUE, FALSE))
  task$task.desc = makeOneClassTaskDesc(id, data, target, weights, blocking, positive)
  addClasses(task, "OneClassTask")
}

makeOneClassTaskDesc = function(id, data, target, weights, blocking, positive) {
  levs = levels(data[[target]])
  m = length(levs)
  if (is.na(positive)) {
    if (m <= 2L) {
      positive = names(which.max(table(data[[target]])))
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
  if (length(td$class.levels) == 2L)
    td$negative = setdiff(td$class.levels, td$positive)
  # else if (length(td$class.levels) == 1L) {
  #   td$class.levels = c(td$positive, setdiff(c(TRUE, FALSE),  td$positive))
  #   td$negative = setdiff(td$class.levels, positive)
  # }
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
  catf("Note: As oneclass classification problem is an unsupervised learning problem,
    the label TRUE and FALSE aren't the ground truth, if the class column is automatecially created by mlR,
    but rather an assumption of the oneclass classification problem.")
}

