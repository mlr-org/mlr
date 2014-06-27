#' @export
#' @param surv.type [\code{character(1)}]\cr
#'  Survival type. Allowed are \dQuote{right} (default), \dQuote{left} and \dQuote{interval2}.
#'  See \code{\link[survival]{Surv}} for details.
#' @rdname SupervisedTask
makeSurvTask = function(id, data, target, surv.type = "right", weights = NULL, blocking = NULL,
  fixup.data = "warn", check.data = TRUE) {
  assertChoice(fixup.data, choices = c("no", "quiet", "warn"))
  assertFlag(check.data)

  task = addClasses(makeSupervisedTask("surv", data, target, weights, blocking), "SurvTask")
  ### FIXME
  ### convert survival times to interval2

  if (fixup.data != "no")
    fixupData(task, target, fixup.data)
  if (check.data)
    checkTask(task, target)
  id = checkOrGuessId(id, data)
  task$task.desc = makeTaskDesc.SurvTask(task, id, target, surv.type)
  return(task)
}

#' @export
checkTask.SurvTask = function(task, target, ...) {
  NextMethod("checkTask")
  assertCharacter(target, len = 2L, any.missing = FALSE)
  ### TODO: more checks here
}

#' @export
fixupData.SurvTask = function(task, target, choice, ...) {
  NextMethod("fixupData")
  if (is.integer(task$env$data[[target[1L]]]))
    task$env$data[[target[1L]]] = as.numeric(task$env$data[[target[1L]]])
  if (!is.logical(task$env$data[[target[2L]]]) || is.numeric(task$env$data[[target[2L]]]))
    task$env$data[[target[2L]]] = as.integer(as.logical(task$env$data[[target[2L]]]))
}

#' @export
makeTaskDesc.SurvTask = function(task, id, target, surv.type) {
  td = makeTaskDescInternal(task, "surv", id, target)
  td$surv.type = surv.type
  addClasses(td, "TaskDescSurv")
}
