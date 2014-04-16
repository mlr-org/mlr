#' @export
#' @rdname SupervisedTask
makeRegrTask = function(id, data, target, weights = NULL, blocking = NULL,
  fixup.data = "warn", check.data = TRUE) {

  task = makeSupervisedTask("RegrTask", "regr", data, target, weights, blocking,
    checkTargetRegr, fixup.data, fixupDataRegr, check.data)
  id = checkOrGuessId(id, data)
  task$task.desc = makeTaskDesc.RegrTask(task, id, target)
  return(task)
}

checkTargetRegr = function(data, target) {
  # these can all be auto-converted in a sane way
  checkTarget("regr", data, target, 1L, list(c("numeric", "integer")))
}

# normal fixup + convert target col to numeric
fixupDataRegr = function(data, target, choice) {
  data = fixupData(data, target, choice)
  targetcol = data[, target]
  if (is.integer(targetcol))
    data[, target] = as.numeric(targetcol)
  return(data)
}

#' @S3method makeTaskDesc RegrTask
makeTaskDesc.RegrTask = function(task, id, target) {
  addClasses(makeTaskDescInternal(task, "regr", id, target), "TaskDescRegr")
}


