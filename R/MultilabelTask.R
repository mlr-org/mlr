#' @export
#' @rdname Task
makeMultilabelTask = function(id, data, target, weights = NULL, blocking = NULL,
  positive = NA_character_, fixup.data = "warn", check.data = TRUE){
  assertChoice(fixup.data, choices = c("no", "quiet", "warn"))
  assertFlag(check.data)
  qassert(target, "S>1")
  task = addClasses(makeSupervisedTask("multilabel", data, target, weights, blocking), "MultilabelTask")
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
    assertLogical(task$env$data[[i]], any.missing = FALSE, .var.name = target[i])
  }
}

#' @export
print.MultilabelTask = function(x, ...) {
  y = getTaskTargets(x)
  sums = colSums(y)
  print.SupervisedTask(x)
  catf("Classes: %i", ncol(y))
  print(sums)
}

makeTaskDesc.MultilabelTask = function(task, id, target) {
  levs = target
  td = makeTaskDescInternal(task, "multilabel", id, target)
  td$class.levels = levs
  return(addClasses(td, "TaskDescMultilabel"))
}
