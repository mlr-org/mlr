#' @export
#' @rdname SupervisedTask
makeClassifTask = function(id, data, target, weights = NULL, blocking = NULL,
  positive, fixup.data = "warn", check.data = TRUE) {
  assertChoice(fixup.data, choices = c("no", "quiet", "warn"))
  assertFlag(check.data)

  task = addClasses(makeSupervisedTask("classif", data, target, weights, blocking), "ClassifTask")
  if (fixup.data != "no")
    fixupData(task, target, fixup.data)
  if (check.data)
    checkTaskCreation(task, target)

  # we expect the target to be a factor from here on
  levs = levels(data[, target])
  m = length(levs)
  if (missing(positive)) {
    if (m <= 2L)
      positive = levs[1L]
    else
      positive = NA_character_
  } else {
    if (m > 2L)
      stop("Cannot set a positive class for a multiclass problem!")
    assertChoice(positive, choices = levs)
  }
  id = checkOrGuessId(id, data)
  task$task.desc = makeTaskDesc.ClassifTask(task, id, target, positive)
  return(task)
}

#' @export
checkTaskCreation.ClassifTask = function(task, target, ...) {
  NextMethod("checkTaskCreation")
  assertString(target)
  if (!is.factor(task$env$data[[target]])) {
    stopf("Target column '%s' must be a factor", target)
  }
}

#' @export
fixupData.ClassifTask = function(task, target, choice, ...) {
  NextMethod("fixupData")
  x = task$env$data[[target]]
  if (is.character(x) || is.logical(x) || is.integer(x))
    task$env$data[[target]] = as.factor(x)
}

#' @export
makeTaskDesc.ClassifTask = function(task, id, target, positive) {
  td = makeTaskDescInternal(task, "classif", id, target)
  td$class.levels = levels(task$env$data[, target])
  td$positive = positive
  td$negative = NA_character_
  if (length(td$class.levels) == 1L)
    td$negative = paste0("not_", positive)
  else if(length(td$class.levels) == 2L)
    td$negative = setdiff(td$class.levels, positive)
  return(addClasses(td, "TaskDescClassif"))
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
