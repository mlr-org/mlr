#' @export
#' @param surv.type [\code{character(1)}]\cr
#'  Survival type. Allowed are \dQuote{right} (default), \dQuote{left} and \dQuote{interval2}.
#'  See \code{\link[survival]{Surv}} for details.
#' @rdname Task
makeSurvTask = function(id, data, target, surv.type = "right", weights = NULL, blocking = NULL,
  fixup.data = "warn", check.data = FALSE) {
  assertChoice(fixup.data, choices = c("no", "quiet", "warn"))
  assertFlag(check.data)
  assertChoice(surv.type, choices = c("right", "left", "interval"))

  # conversion to interval2
  if (surv.type == "right") {
    time1 = as.numeric(data[[target[1L]]])
    event = as.integer(data[[target[2L]]])
    
    time2 = numeric(length(time1))
    time2[event == 0L] = Inf
    time2[event == 1L] = time1[event == 1L]
    
    data[[target[1L]]] = data[[target[2L]]] = NULL
    data = cbind(time1, time2, data)
    target = c("time1", "time2")
  } else if (surv.type == "left") {
    time2 = as.numeric(data[[target[1L]]])
    event = as.integer(data[[target[2L]]])
    
    time1 = numeric(length(time2))
    time1[event == 0L] = -Inf
    time1[event == 1L] = time2[event == 1L]
    
    data[[target[1L]]] = data[[target[2L]]] = NULL
    data = cbind(time1, time2, data)
    target = c("time1", "time2")
  } 
  
  task = addClasses(makeSupervisedTask("surv", data, target, weights, blocking), "SurvTask")
  
  if (fixup.data != "no")
    fixupData(task, target, fixup.data)
  # FIXME: checkTaskCreation doesn't allow infinite values. We need this though.
  if (check.data)
    checkTaskCreation(task, target)
  id = checkOrGuessId(id, data)
  task$task.desc = makeTaskDesc.SurvTask(task, id, target, surv.type)
  return(task)
}

checkTaskCreation.SurvTask = function(task, target, ...) {
  NextMethod("checkTaskCreation")
  assertCharacter(target, len = 2L, any.missing = FALSE)
  ### TODO: more checks here
}

fixupData.SurvTask = function(task, target, choice, ...) {
  NextMethod("fixupData")
  if (is.integer(task$env$data[[target[1L]]]))
    task$env$data[[target[1L]]] = as.numeric(task$env$data[[target[1L]]])
  #if (!is.logical(task$env$data[[target[2L]]]) || is.numeric(task$env$data[[target[2L]]]))
  #  task$env$data[[target[2L]]] = as.integer(as.logical(task$env$data[[target[2L]]]))
}

makeTaskDesc.SurvTask = function(task, id, target, surv.type) {
  td = makeTaskDescInternal(task, "surv", id, target)
  td$surv.type = surv.type
  addClasses(td, "TaskDescSurv")
}
