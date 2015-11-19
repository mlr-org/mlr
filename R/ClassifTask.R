#' @export
#' @rdname Task
makeClassifTask = function(id = deparse(substitute(data)), data, target, weights = NULL, blocking = NULL, positive = NA_character_, fixup.data = "warn", check.data = TRUE) {
  assertString(id)
  assertDataFrame(data)
  assertString(target)
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
    } else if (is.factor(x) && fixup.data == "warn" && any(table(x) == 0L)) {
      warningf("Target column '%s' contains empty factor levels", target)
      data[[target]] = droplevels(x)
    }
  }

  task = makeSupervisedTask("classif", data, target, weights, blocking, fixup.data = fixup.data, check.data = check.data)

  if (check.data) {
    assertFactor(data[[target]], any.missing = FALSE, empty.levels.ok = FALSE, .var.name = target)
  }

  task$task.desc = makeTaskDesc.ClassifTask(task, id, target, positive)
  addClasses(task, "ClassifTask")
}

makeTaskDesc.ClassifTask = function(task, id, target, positive) {
  levs = levels(task$env$data[[target]])
  m = length(levs)
  if (is.na(positive)) {
    if (m <= 2L)
      positive = levs[1L]
  } else {
    if (m > 2L)
      stop("Cannot set a positive class for a multiclass problem!")
    assertChoice(positive, choices = levs)
  }
  td = makeTaskDescInternal(task, "classif", id, target)
  td$class.levels = levs
  td$positive = positive
  td$negative = NA_character_
  if (length(td$class.levels) == 1L)
    td$negative = paste0("not_", positive)
  else if(length(td$class.levels) == 2L)
    td$negative = setdiff(td$class.levels, positive)
  return(addClasses(td, c("TaskDescClassif", "TaskDescSupervised")))
}

#' @export
print.ClassifTask = function(x, ...) {
  # remove 1st newline
  di = printToChar(table(getTaskTargets(x)), collapse = NULL)[-1L]
  m = length(x$task.desc$class.levels)
  print.SupervisedTask(x)
  catf("Classes: %i", m)
  catf(collapse(di, "\n"))
  catf("Positive class: %s", x$task.desc$positive)
}
