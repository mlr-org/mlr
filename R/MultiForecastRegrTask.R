#' @export
#' @rdname Task
#' @importFrom zoo index coredata
makeMultiForecastRegrTask = function(id = deparse(substitute(data)), data, target,
                                weights = NULL, blocking = NULL, fixup.data = "warn",
                                check.data = TRUE, frequency = 1L) {
  assertString(id)
  assertClass(data,"xts")
  assertString(target)
  assertChoice(target, c("all", colnames(data)))
  assertInteger(frequency, lower = 0L, max.len = 1L)
  assertChoice(fixup.data, choices = c("no", "quiet", "warn"))
  assertFlag(check.data)


  is.target.all = target == "all"
  row.names = index(data)
  col.names = colnames(data)
  assert(
    checkClass(row.names, "POSIXlt"),
    checkClass(row.names, "POSIXt"),
    checkClass(row.names, "POSIXct")
    )
  data <- data.frame(row.names = row.names, coredata(data))

  if (fixup.data != "no" && all(!is.target.all)) {
    if (is.integer(data[[target]]))
      data[[target]] = as.double(data[[target]])
  } else if (fixup.data != "no"){
    is.int = vapply(data,is.integer,TRUE)
    if (all(is.int)){
      data = lapply(data,as.double)
      data = as.data.frame(row.names = row.names, data)
    }
  }

  if (is.target.all)
    target = colnames(data)

  task = makeSupervisedTask("mfcregr", data, target, weights, blocking, fixup.data = fixup.data, check.data = check.data)

  if (check.data && !is.target.all) {
    assertNumeric(data[[target]], any.missing = FALSE, finite = TRUE, .var.name = target)
  } else if (check.data && is.target.all){
    sapply(data,assertNumeric, any.missing = FALSE, finite = TRUE)
  }

  task$task.desc = makeTaskDesc.MultiForecastRegrTask(task, id, target, frequency)
  addClasses(task, c("MultiForecastRegrTask","TimeTask"))
}

makeTaskDesc.MultiForecastRegrTask = function(task, id, target, frequency) {
  td = makeTaskDescInternal(task, "mfcregr", id, target, time = TRUE)
  td$dates = c(rownames(task$env$data)[1],rownames(task$env$data)[nrow(task$env$data)])
  if (missing(frequency))
    frequency = task$task.desc$frequency
  td$frequency = frequency
  td$col.names = colnames(task$env$data)
  addClasses(td, c("TaskDescMultiForecastRegr", "TaskDescSupervised"))
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

