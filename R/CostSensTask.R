#' @export
#' @rdname SupervisedTask
makeCostSensTask = function(id, data, costs, blocking = NULL, fixup.data = "warn", check.data = TRUE) {
  checkArg(costs, c("data.frame", "matrix"), na.ok = FALSE)
  if (is.data.frame(costs))
    costs = as.matrix(costs)
  checkArg(costs, "matrix", na.ok = FALSE, lower = 0)
  if (is.null(colnames(costs)))
    colnames(costs) = paste("y", seq_col(costs), sep = "")
  if (check.data) {
    checkColumnNames(costs)
  }
  task = makeSupervisedTask("CostSensTask", "costsens", data, character(0L), NULL, blocking,
    checkTargetCostSens, fixup.data, fixupData, check.data)
  if (check.data) {
    # check costs a bit more
    if (nrow(costs) != nrow(data))
    stopf("Number of rows in cost matrix (%s) should equal the number of observations (%s).",
      nrow(costs), nrow(data))
    # we use ..y.. later in the models as a name for temp labels
    if ("..y.." %in% c(colnames(data), colnames(data)))
      stopf("The name '..y..' is currently reserved for costsens tasks. You can use it neither for features nor labels!")
  }
  task$env$costs = costs
  id = checkOrGuessId(id, data)
  task$task.desc = makeTaskDesc.CostSensTask(task, id, character(0L))
  return(task)
}

# nothing to check, we force target = char(0) in constructor
# cost matrix is checked in constructor
checkTargetCostSens = function(data, target) {}

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

