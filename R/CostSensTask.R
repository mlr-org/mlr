#' @export
#' @rdname SupervisedTask
makeCostSensTask = function(id, data, costs, blocking = NULL, fixup.data = "warn", check.data = TRUE) {
  checkArg(costs, "matrix", na.ok = FALSE)
  if (is.null(colnames(costs)))
    colnames(costs) = paste("y", seq_col(costs), sep = "")
  checkColumnNames(costs)
  task = makeSupervisedTask("CostSensTask", "costsens", data, character(0L), NULL, blocking,
    checkTargetCostSens, fixup.data, fixupData, check.data)
  # check costs a bit more and store them
  if (nrow(costs) != nrow(data))
    stopf("Number of rows in cost matrix (%s) should equal the number of observations (%s).",
      nrow(costs), nrow(data))
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

