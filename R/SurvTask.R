#' @export
#' @rdname SupervisedTask
makeSurvTask = function(id, data, target, weights = NULL, blocking = NULL,
  fixup.data = "warn", check.data = TRUE) {

  task = makeSupervisedTask("SurvTask", "surv", data, target, weights, blocking,
    checkTargetSurv, fixup.data, fixupDataSurv, check.data)
  id = checkOrGuessId(id, data)
  task$task.desc = makeTaskDesc.SurvTask(task, id, target)
  return(task)
}

checkTargetSurv = function(data, target) {
  checkTarget("surv", data, target, 2L, list(c("numeric", "integer"), c("logical", "integer")))
}

# normal fixup + convert target cols numeric (time) and 0-1-integer (events)
fixupDataSurv = function(data, target, choice) {
  data = fixupData(data, target, choice)
  if (is.integer(data[[target[1L]]]))
    data[[target[1L]]] = as.numeric(data[[target[1L]]])
  if (!is.logical(data[[target[2L]]]) || is.integer(data[[target[2L]]]))
    data[[target[2L]]] = as.integer(as.logical(data[[target[2L]]]))
  return(data)
}

#' @S3method makeTaskDesc SurvTask
makeTaskDesc.SurvTask = function(task, id, target) {
  addClasses(makeTaskDescInternal(task, "surv", id, target), "TaskDescSurv")
}
