#' @export
#' @rdname Task
makeCostSensTask = function(id, data, costs, blocking = NULL, fixup.data = "warn", check.data = TRUE) {
  assertChoice(fixup.data, choices = c("no", "quiet", "warn"))
  assertFlag(check.data)

  # we don't have a target nor weights
  target = character(0L)
  weights = NULL
  task = addClasses(makeSupervisedTask("costsens", data, target, weights, blocking), "CostSensTask")
  task$env$costs = costs

  if (fixup.data != "no")
    fixupData(task, target, fixup.data)
  if (check.data)
    checkTaskCreation(task, target)

  id = checkOrGuessId(id, data)
  task$task.desc = makeTaskDesc.CostSensTask(task, id, target)
  return(task)
}

checkTaskCreation.CostSensTask = function(task, target, ...) {
  NextMethod("checkTaskCreation")
  assert(checkMatrix(task$env$costs, any.missing = FALSE), checkDataFrame(task$env$costs, any.missing = FALSE))
  if (is.data.frame(task$env$costs))
    task$env$costs = as.matrix(task$env$costs)
  assertNumeric(task$env$costs, lower = 0)
  if (is.null(colnames(task$env$costs)))
    colnames(task$env$costs) = paste("y", seq_col(task$env$costs), sep = "")
  checkColumnNames(task$env$costs)

  if (nrow(task$env$costs) != nrow(task$env$data))
    stopf("Number of rows in cost matrix (%s) should equal the number of observations (%s).",
      nrow(task$env$costs), nrow(task$env$data))
  # we use ..y.. later in the models as a name for temp labels
  if ("..y.." %in% c(colnames(task$env$data), colnames(task$env$costs)))
    stopf("The name '..y..' is currently reserved for costsens tasks. You can use it neither for features nor labels!")
}

fixupData.CostSensTask = function(task, target, choice, ...) {
  # FIXME: move fixes from checkTaskCreation here?
  NextMethod("fixupData")
}

makeTaskDesc.CostSensTask = function(task, id, target) {
  td = makeTaskDescInternal(task, "costsens", id, target)
  td$class.levels = colnames(task$env$costs)
  return(addClasses(td, "TaskDescCostSens"))
}

#' @export
print.CostSensTask = function(x, ...) {
  print.SupervisedTask(x, print.target = FALSE, print.weights = FALSE)
  levs = x$task.desc$class.levels
  catf("Classes: %i\n%s", length(levs), clipString(collapse(levs, sep = ", "), 30L))
}
