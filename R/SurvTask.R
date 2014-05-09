#' @export
#' @param surv.type [\code{character(1)}]\cr
#'  Survival type. Supported are \dQuote{left}, \dQuote{right} and \dQuote{interval2}.
#'  The first and second \code{target} columns will be passed unnamed to \code{\link[survival]{Surv}}
#'  to create a survival object of type \code{surv.type}.
#' @rdname SupervisedTask
makeSurvTask = function(id, data, target, surv.type = "right", weights = NULL, blocking = NULL,
  fixup.data = "warn", check.data = TRUE) {

  task = makeSupervisedTask("SurvTask", "surv", data, target, weights, blocking,
    checkTargetSurv, fixup.data, fixupDataSurv, check.data)
  id = checkOrGuessId(id, data)
  task$task.desc = makeTaskDesc.SurvTask(task, id, target, surv.type)
  return(task)
}

checkTargetSurv = function(data, target) {
  checkTarget("surv", data, target, 2L, list(c("logical", "numeric", "integer"), c("logical", "numeric", "integer")))
}

# FIXME: this is useless because we cannot pass surv.type.
fixupDataSurv = function(data, target, choice) {
  data = fixupData(data, target, choice)
  if (is.integer(data[[target[1L]]]))
    data[[target[1L]]] = as.numeric(data[[target[1L]]])
  if (!is.logical(data[[target[2L]]]) || is.numeric(data[[target[2L]]]))
    data[[target[2L]]] = as.integer(as.logical(data[[target[2L]]]))
  return(data)
}

#' @S3method makeTaskDesc SurvTask
makeTaskDesc.SurvTask = function(task, id, target, surv.type) {
  td = addClasses(makeTaskDescInternal(task, "surv", id, target), "TaskDescSurv")
  td$surv.type = surv.type
  td
}
