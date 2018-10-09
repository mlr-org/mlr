#' Create an task object for multivariate forecasting.
#'
#' @rdname Task
#' @description Creates a task that is used for multivariate forecasting. Target the name of a single series
#' or 'All'. Targeting a single series will produce a single output similar to univariate tasks while
#' treating all other variables endogeneously. targeting 'All' will forecast all variables forward.
#' @export
makeMultiForecastRegrTask = function(id = deparse(substitute(data)), data, target,
  weights = NULL, blocking = NULL, frequency = 1L, date.col = "dates", fixup.data = "warn",
  check.data = TRUE, coordinates = NULL) {

  assertString(id)
  assertString(target)
  assertChoice(target, c("all", colnames(data)))
  assertInteger(frequency, lower = 0L, max.len = 1L)
  assertChoice(fixup.data, choices = c("no", "quiet", "warn"))
  assertFlag(check.data)


  is.target.all = target == "all"
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
  dates = data[, date.col, drop = FALSE]

  if (check.data) {
    if (!is.target.all) {
      assertNumeric(data[[target]], any.missing = FALSE, finite = TRUE, .var.name = target)
    } else if (is.target.all) {
      lapply(data, assertNumeric, any.missing = FALSE, finite = TRUE)
    }
    if (any(duplicated(dates)))
      stop(catf("Multiple observations for %s. Dates must be unique.", dates[any(duplicated(dates)), ]))
    if (!is.POSIXt(dates[, 1]))
      stop(catf("Dates are of type %s, but must be in a POSIXt format", class(dates[, 1])))
  }
  if (fixup.data != "no" && all(!is.target.all)) {
    if (is.integer(data[[target]]))
      data[[target]] = as.double(data[[target]])
    if (is.unsorted(dates[, 1])) {
      if (fixup.data == "warn")
        warning("Dates and data will be sorted in ascending order")
      date.order = order(dates)
      data = data[date.order, , drop = FALSE]
      dates = dates[date.order, , drop = FALSE]
    }
  }
  # Remove the date column
  data = data[, date.col != colnames(data), drop = FALSE]

  if (check.data) {
    if (is.target.all) {
      vapply(data, assertNumeric, any.missing = FALSE, finite = TRUE, FUN.VALUE = rep(1.0, dim(data)[1]))
    } else {
      assertNumeric(data[[target]], any.missing = FALSE, finite = TRUE, .var.name = target)
    }
    if (any(duplicated(dates)))
      stop(catf("Multiple observations for %s. Dates must be unique.", dates[any(duplicated(dates)), ]))
    if (!is.POSIXt(dates[, 1]))
      stop(catf("Dates are of type %s, but must be in a POSIXt format", class(dates[, 1])))
  }

  if (is.target.all)
    target = colnames(data)
  task = makeSupervisedTask("mfcregr", data, target, weights, blocking, fixup.data = fixup.data, check.data = check.data, coordinates)

  task$task.desc = makeMultiForecastRegrTaskDesc(id, data, target, weights, blocking, frequency, dates, coordinates)
  addClasses(task, c("MultiForecastRegrTask", "TimeTask"))
}

makeMultiForecastRegrTaskDesc = function(id, data, target, weights, blocking, frequency, dates, coordinates) {
  td = makeTaskDescInternal("mfcregr", id, data, target, weights, blocking, coordinates)
  td$dates = dates
  td$frequency = frequency
  addClasses(td, c("ForecastRegrTaskDesc", "SupervisedTaskDesc"))
}


#' @export
print.MultiForecastRegrTask = function(x, print.weights = TRUE, ...) {
  td = getTaskDesc(x)
  catf("Task: %s", td$id)
  catf("Type: %s", td$type)
  catf("Target: %s", stri_paste(td$target, collapse = " "))
  catf("Observations: %i", td$size)
  catf("Dates:\n Start: %s \n End:   %s", td$dates[1,], td$dates[nrow(td$dates),])
  catf("Frequency: %i", td$frequency)
  catf("Features:")
  catf(printToChar(td$n.feat, collapse = "\n"))
  catf("Missings: %s", td$has.missings)
  if (print.weights)
    catf("Has weights: %s", td$has.weights)
  catf("Has blocking: %s", td$has.blocking)
}

