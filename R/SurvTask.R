#' @rdname Task
#' @param censoring [\code{character(1)}]\cr
#'  Censoring type. Allowed choices are \dQuote{rcens} for right censored data (default),
#'  \dQuote{lcens} for left censored and \dQuote{icens} for interval censored data using
#'  the \dQuote{interval2} format.
#'  See \code{\link[survival]{Surv}} for details.
#' @export
makeSurvTask = function(id, data, target, censoring = "rcens", weights = NULL, blocking = NULL,
  fixup.data = "warn", check.data = TRUE) {
  assertChoice(censoring, choices = c("rcens", "lcens", "icens"))
  assertChoice(fixup.data, choices = c("no", "quiet", "warn"))
  assertFlag(check.data)

  task = addClasses(makeSupervisedTask("surv", data, target, weights, blocking), "SurvTask")

  if (fixup.data != "no")
    fixupData(task, target, fixup.data, censoring = censoring)
  if (check.data)
    checkTaskCreation(task, target, censoring = censoring)
  id = checkOrGuessId(id, data)
  task$task.desc = makeTaskDesc.SurvTask(task, id, target, censoring)
  return(task)
}

checkTaskCreation.SurvTask = function(task, target, ..., censoring) {
  NextMethod("checkTaskCreation")
  assertCharacter(target, len = 2L, any.missing = FALSE)
  if (censoring %in% c("lcens", "rcens")) {
    time = task$env$data[[target[1L]]]
    event = task$env$data[[target[2L]]]
    assertNumeric(time, lower = 0, finite = TRUE, any.missing = FALSE, .var.name = "target column time")
    assertLogical(event, any.missing = FALSE, .var.name = "target column event")
  } else { # icens
    time1 = task$env$data[[target[1L]]]
    time2 = task$env$data[[target[2L]]]
    assertNumeric(time1, any.missing = TRUE, finite = FALSE, .var.name = "target column time1")
    assertNumeric(time2, any.missing = TRUE, finite = FALSE, .var.name = "target column time2")
  }
}

fixupData.SurvTask = function(task, target, choice, ..., censoring) {
  NextMethod("fixupData")
  if (censoring %in% c("lcens", "rcens")) {
    time = task$env$data[[target[1L]]]
    event = task$env$data[[target[2L]]]

    if (is.integer(time)) {
      task$env$data[[target[1L]]] = as.double(time)
    }

    if (is.numeric(event)) {
      if (testIntegerish(event) && all(as.integer(event) %in% c(0L, 1L)))
        task$env$data[[target[2L]]] = (as.integer(event) == 1L)
    } else if (is.factor(event)) {
      lvls = levels(event)
      if (length(lvls) == 2L) {
        if (all(lvls %in% c("TRUE", "FALSE"))) {
          task$env$data[[target[2L]]] = (event == "TRUE")
        } else if (all(lvls) %in% c("0", "1")) {
          task$env$data[[target[2L]]] = (event == "1")
        }
      }
    }
  } else { # icens
    time1 = task$env$data[[target[1L]]]
    time2 = task$env$data[[target[2L]]]

    if (is.integer(time1))
      task$env$data[[target[1L]]] = as.numeric(time1)
    if (is.integer(time2))
      task$env$data[[target[2L]]] = as.numeric(time2)
  }
}

makeTaskDesc.SurvTask = function(task, id, target, censoring) {
  td = makeTaskDescInternal(task, "surv", id, target)
  td$censoring = censoring
  addClasses(td, "TaskDescSurv")
}
