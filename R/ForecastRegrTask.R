#' @export
#' @rdname Task
#' @importFrom zoo index coredata
makeForecastRegrTask = function(id = deparse(substitute(data)), data, target,
                                weights = NULL, blocking = NULL, fixup.data = "warn",
                                check.data = TRUE, frequency = 1L) {
  assertString(id)
  assertClass(data,"xts")
  assertString(target)
  assertInteger(frequency, lower = 0L, max.len = 1L)
  assertChoice(fixup.data, choices = c("no", "quiet", "warn"))
  assertFlag(check.data)

  data <- data.frame(row.names = index(data), coredata(data))

  if (fixup.data != "no") {
    if (is.integer(data[[target]]))
      data[[target]] = as.double(data[[target]])
  }

  task = makeSupervisedTask("fcregr", data, target, weights, blocking, fixup.data = fixup.data, check.data = check.data)

  if (check.data) {
    assertNumeric(data[[target]], any.missing = FALSE, finite = TRUE, .var.name = target)
  }

  task$task.desc = makeTaskDesc.ForecastRegrTask(task, id, target, frequency)
  addClasses(task, c("ForecastRegrTask","TimeTask"))
}

makeTaskDesc.ForecastRegrTask = function(task, id, target, frequency) {
  td = makeTaskDescInternal(task, "fcregr", id, target, time = TRUE)
  td$dates = c(rownames(task$env$data)[1],rownames(task$env$data)[nrow(task$env$data)])
  if (missing(frequency))
    frequency = task$task.desc$frequency
  td$frequency = frequency
  addClasses(td, c("TaskDescForecastRegr", "TaskDescSupervised"))
}


#' @export
print.ForecastRegrTask = function(x, print.weights = TRUE, ...) {
  td = x$task.desc
  catf("Task: %s", td$id)
  catf("Type: %s", td$type)
  catf("Observations: %i", td$size)
  catf("Dates:\n Start: %s \n End:   %s", td$dates[1], td$dates[2])
  catf("Frequency: %i", td$frequency)
  catf("Features:")
  catf(printToChar(td$n.feat, collapse = "\n"))
  catf("Missings: %s", td$has.missings)
  if (print.weights)
    catf("Has weights: %s", td$has.weights)
  catf("Has blocking: %s", td$has.blocking)
}

