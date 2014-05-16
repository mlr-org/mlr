#' @export
#' @rdname SupervisedTask
makeCostSensTask = function(id, data, costs, blocking = NULL, fixup.data = "warn", check.data = TRUE) {
  checkArg(fixup.data, choices = c("no", "quiet", "warn"))
  checkArg(check.data, "logical", len = 1L, na.ok = FALSE)

  # we don't have a target nor weights
  target = character(0L)
  weights = NULL
  task = addClasses(makeSupervisedTask("costsens", data, target, weights, blocking), "CostSensTask")
  task$env$costs = costs

  if (fixup.data != "no")
    fixupData(task, target, fixup.data)
  if (check.data)
    checkTask(task, target)

  id = checkOrGuessId(id, data)
  task$task.desc = makeTaskDesc.CostSensTask(task, id, target)
  return(task)
}

#' @S3method checkTask CostSensTask
checkTask.CostSensTask = function(task, target, ...) {
  NextMethod("checkTask")

  checkArg(task$env$costs, c("data.frame", "matrix"), na.ok = FALSE)
  if (is.data.frame(task$env$costs))
    task$env$costs = as.matrix(task$env$costs)
  checkArg(task$env$costs, "matrix", na.ok = FALSE, lower = 0)
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

#' @S3method fixupData CostSensTask
fixupData.CostSensTask = function(task, target, choice, ...) {
  # FIXME: move fixes from checkTask here?
  NextMethod("fixupData")
}

#' @S3method makeTaskDesc CostSensTask
makeTaskDesc.CostSensTask = function(task, id, target) {
  td = makeTaskDescInternal(task, "costsens", id, target)
  td$class.levels = colnames(task$env$costs)
  return(addClasses(td, "TaskDescCostSens"))
}

#' @S3method print CostSensTask
print.CostSensTask = function(x, ...) {
  print.SupervisedTask(x, print.target = FALSE, print.weights = FALSE)
  levs = x$task.desc$class.levels
  catf("Classes: %i\n%s", length(levs), clipString(collapse(levs, sep = ", "), 30L))
}
