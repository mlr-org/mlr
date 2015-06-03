#' @export
#' @rdname Task
makeMultilabelTask = function(id, data, target, weights=NULL, blocking=NULL,
                              positive = NA_character_, fixup.data = "warn", check.data=TRUE){
  assertChoice(fixup.data, choices = c("no", "quiet", "warn"))
  assertFlag(check.data)
  assertCharacter(target)
  task = addClasses(makeSupervisedTask("multilabel", data, target,
                                       weights, blocking), "MultilabelTask")
  if (fixup.data != "no")
    fixupData(task,target,fixup.data) 
  if (check.data)
    checkTaskCreation(task,target) 
  id = checkOrGuessId(id, data)
  task$task.desc = makeTaskDesc.MultilabelTask(task, id, target)
  return(task)
}

checkTaskCreation.MultilabelTask = function(task, target, ...) {
  NextMethod("checkTaskCreation")
  for (i in target){
    assertFactor(task$env$data[[i]], any.missing = FALSE, empty.levels.ok = FALSE, .var.name = target[i])
  }
}

#' @export
print.MultilabelTask = function(x, ...) {
  # remove 1st newline
  di = printToChar(table(getTaskTargets(x)), collapse = NULL)[-1L]
  m = length(x$task.desc$class.levels)
  print.SupervisedTask(x)
  catf("Classes: %i", m)
  catf(collapse(di, "\n"))
  catf("Positive class: %s", x$task.desc$positive)
}

fixupData.MultilabelTask = function(task, target, choice, ...) {
  NextMethod("fixupData")
  for (i in target){
    x = task$env$data[[i]]
    if (is.character(x) || is.logical(x) || is.integer(x))
      task$env$data[[i]] = as.factor(x)
  }
}

makeTaskDesc.MultilabelTask = function(task,id,target){
  levs = lapply(task$env$data[target],levels)
  m = lapply(levs,length)
  td = makeTaskDescInternal(task, "multilabel", id, target)
  td$class.levels = levs
  return(addClasses(td, "TaskDescMultilabel"))
}
