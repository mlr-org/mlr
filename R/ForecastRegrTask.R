#' @title Create a task for univariate forecasting
#'
#' @rdname Task
#'
#' @description Creates a task for univariate forecasting learners
#'
#' @export
makeForecastRegrTask = function(id = deparse(substitute(data)), data, target,
  weights = NULL, blocking = NULL, frequency = 1L, date.col = "dates", fixup.data = "warn",
  check.data = TRUE, coordinates = NULL) {

  assertString(id)
  assertString(target)
  assertString(date.col)
  frequency = asCount(frequency)
  assertChoice(fixup.data, choices = c("no", "quiet", "warn"))
  assertFlag(check.data)

  if (class(data)[1] != "data.frame") {
    warningf("Provided data for task is not a pure data.frame but from class %s, hence it will be converted.",  class(data)[1])
    if (class(data)[1] == "xts") {
      date.vals = index(data)
      data = as.data.frame(data)
      data[["dates"]] = date.vals
      date.col = "dates"
    } else {
      data = as.data.frame(data)
    }
  }
  assertDataFrame(data)
  # Need to check that dates
  # 1. Exist
  # 2. Are unique
  # 3. Follow POSIXct convention
  dates = data[, date.col, drop = TRUE]
  if (check.data) {
    assertNumeric(data[[target]], any.missing = FALSE, finite = TRUE, .var.name = target)
    if (any(duplicated(dates)))
      stop(catf("Multiple observations for %s. Dates must be unique.", dates[any(duplicated(dates))]))
    if (!is.POSIXt(dates))
      stop(catf("Dates are of type %s, but must be in a POSIXt format", class(dates[1])))
  }
  if (fixup.data != "no") {
    if (is.integer(data[[target]]))
      data[[target]] = as.double(data[[target]])
    if (is.unsorted(dates)) {
      if (fixup.data == "warn")
        warning("Dates and data will be sorted in ascending order")
      date.order = order(dates)
      data = data[date.order, , drop = FALSE]
      dates = dates[date.order, , drop = FALSE]
    }
  }
  # Remove the date column
  data = data[, date.col != colnames(data), drop = FALSE]

  task = makeSupervisedTask("fcregr", data, target, weights, blocking,
    fixup.data = fixup.data, check.data = check.data, coordinates = coordinates)
  task$task.desc = makeForecastRegrTaskDesc(id, data, target, weights, blocking, frequency, dates, coordinates)
  addClasses(task, c("ForecastRegrTask", "TimeTask"))
}

makeForecastRegrTaskDesc = function(id, data, target, weights, blocking, frequency, dates, coordinates) {
  td = makeTaskDescInternal("fcregr", id, data, target, weights, blocking, coordinates)
  td$dates = dates
  td$frequency = frequency
  addClasses(td, c("ForecastRegrTaskDesc", "SupervisedTaskDesc"))
}


#' @export
print.ForecastRegrTask = function(x, print.weights = TRUE, ...) {
  td = getTaskDesc(x)
  catf("Task: %s", td$id)
  catf("Type: %s", td$type)
  catf("Target: %s", td$target)
  catf("Observations: %i", td$size)
  catf("Dates:\n Start: %s \n End:   %s", td$dates[1], td$dates[length(td$dates)])
  catf("Frequency: %i", td$frequency)
  catf("Features:")
  catf(printToChar(td$n.feat, collapse = "\n"))
  catf("Missings: %s", td$has.missings)
  if (print.weights)
    catf("Has weights: %s", td$has.weights)
  catf("Has blocking: %s", td$has.blocking)
}

