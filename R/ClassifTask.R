#' @export
#' @rdname SupervisedTask
makeClassifTask = function(id, data, target, weights = NULL, blocking = NULL,
  positive, fixup.data = "warn", check.data = TRUE) {

  task = makeSupervisedTask("ClassifTask", "classif", data, target, weights, blocking,
    checkTargetClassif, fixup.data, fixupDataClassif, check.data)

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
    checkArg(positive, choices = levs)
  }
  id = checkOrGuessId(id, data)
  task$task.desc = makeTaskDesc.ClassifTask(task, id, target, positive)
  return(task)
}

checkTargetClassif = function(data, target) {
  # these can all be auto-converted in a sane way
  checkTarget("classif", data, target, 1L, list(c("factor", "logical", "character", "integer")))
}

# normal fixup + convert target col to factor
fixupDataClassif = function(data, target, choice) {
  data = fixupData(data, target, choice)
  targetcol = data[, target]
  if (is.character(targetcol) || is.logical(targetcol) || is.integer(targetcol))
    data[, target] = as.factor(targetcol)
  return(data)
}

#' @S3method makeTaskDesc ClassifTask
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

#' @S3method print ClassifTask
print.ClassifTask = function(x, ...) {
  # remove 1st newline
  di = printToChar(table(getTaskTargets(x)), collapse = NULL)[-1L]
  m = length(x$task.desc$class.levels)
  print.SupervisedTask(x)
  catf("Classes: %i", m)
  catf(collapse(di, "\n"))
  catf("Positive class: %s", x$task.desc$positive)
}
