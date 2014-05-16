#' @export
#' @rdname SupervisedTask
makeSurvTask = function(id, data, target, weights = NULL, blocking = NULL,
  fixup.data = "warn", check.data = TRUE) {
  checkArg(fixup.data, choices = c("no", "quiet", "warn"))
  checkArg(check.data, "logical", len = 1L, na.ok = FALSE)

  task = addClasses(makeSupervisedTask("surv", data, target, weights, blocking), "SurvTask")

  if (fixup.data != "no")
    fixupData(task, target, fixup.data)
  if (check.data)
    checkTask(task, target)
  id = checkOrGuessId(id, data)
  task$task.desc = makeTaskDesc.SurvTask(task, id, target)
  return(task)
}

#' @S3method checkTask SurvTask
checkTask.SurvTask = function(task, target, ...) {
  NextMethod("checkTask")
  checkArg(target, "character", len = 2L)
  ### TODO: more checks here
}

#' @S3method fixupData SurvTask
fixupData.SurvTask = function(task, target, choice, ...) {
  NextMethod("fixupData")
  if (is.integer(task$env$data[[target[1L]]]))
    task$env$data[[target[1L]]] = as.numeric(data$env$data[[target[1L]]])
  if (!is.logical(task$env$data[[target[2L]]]) || is.numeric(task$env$data[[target[2L]]]))
    task$env$data[[target[2L]]] = as.integer(as.logical(task$env$data[[target[2L]]]))
}

#' @S3method makeTaskDesc SurvTask
makeTaskDesc.SurvTask = function(task, id, target) {
  addClasses(makeTaskDescInternal(task, "surv", id, target), "TaskDescSurv")
}
