# dates has to be defined to avoid a warning in R CMD CHECK
globalVariables("dates")
#' @title Generate lags and differences for feature variables
#'
#' @description Replace all variables with their generated lagged and differenced variables.
#'
#' @template arg_taskdf
#' @template arg_taskdf_target
#' @param lag [\code{integer}]\cr
#' An integer vector of lag lengths.
#' @param difference [\code{integer}]\cr
#' An integer of the order of differencing
#' @param cols [\code{character}]\cr
#' A character vector of columns to create lag features for.
#' Default is to use all columns.
#' @param seasonal.cols [\code{character}]\cr
#' A character vector of columns to create seasonal lag features for. Defaults to all columns
#' @param seasonal.lag [\code{integer}]\cr
#' An integer vector of seasonal lag lengths, made as \code{seasonal.lag * frequency}
#' @param seasonal.difference [\code{integer}]\cr
#' An integer of the seasonal order of difference, made as \code{seasonal.difference * frequency}
#' @param add_var [\code{logical}]
#' When TRUE, creates the lagged rolling variance based on the \code{lag}
#' @param frequency [\code{integer}]\cr
#' An integer representing the periodicity in the time series. If frequency is declared in the task,
#' the task frequency will be used.
#' @param add_dates [\code{character()}]\cr
#' A character vector of \code{data.table} date functions used to create date variables
#' @param na.pad [\code{logical}]\cr
#' A logical to denote whether the data should be padded to the original size with NAs
#' @param difference.lag [\code{integer}]\cr
#' An integer denoting the period to difference over
#' @param momentum [\code{integer}] \cr
#' An vector of integers denoting the lags for momentum calculation
#' @param
#' @param seasonal.difference.lag [\code{integer}]\cr
#' An integer denoting the period to seasonaly difference over
#' @param return.nonlag [\code{logical}]\cr
#' A logical to denote whether the original unlagged features should be returned
#' @param grouping [\code{character}]\cr
#' The name of the column to be passed to data.table's \code{by} function. This will take lags and differences wrt the groups.
#' @param TTR.funcs [\code{list}]\cr
#' A list of TTR functions such as \code{list(runSum = list(n = 1:10, cumulative = TRUE))}
#' @param date.col [code{POSIXct}]
#' The dates for each observation. In the case of a forecasting task, these will be taken from the task description.
#' @export
#' @family eda_and_preprocess
#' @examples
#' set.seed(1234)
#' dat = data.frame(arma_test = as.numeric(arima.sim(model = list(ar = c(.5,.2), ma = c(.4), order = c(2,0,1)), n = 2000)))
#' times = as.POSIXct("1992-01-14") + 0:1999
#' regr.task = makeRegrTask(id = "Lagged ML model", data = dat, target = "arma_test")
#' regr.task.lag = createLagDiffFeatures(regr.task, lag = 1L:200L, difference = 0L, date.col = times, add_var = TRUE)
#' lrn = makeLearner("regr.lm")
#' trn = train(lrn,regr.task.lag)
#' forecast(trn, h = 5)
createLagDiffFeatures = function(obj, target = character(0L), lag = 0L, difference = 0L, difference.lag = 0L,
  cols = NULL, seasonal.cols = NULL, seasonal.lag = 0L, seasonal.difference = 0L,
  seasonal.difference.lag = 0L, frequency = 1L, add_dates = "yday",
  na.pad = FALSE, return.nonlag = FALSE, grouping = NULL, add_var = FALSE, TTR.funcs = NULL, date.col) {

  assertIntegerish(lag, lower = 0L, upper = 100000L)
  assertIntegerish(difference, lower = 0L, upper = 100000L)
  assertIntegerish(difference.lag, lower = 0L, upper = 100000L)
  assertIntegerish(seasonal.lag, lower = 0L, upper = 100000L)
  assertIntegerish(seasonal.difference, lower = 0L, upper = 100000L)
  assertIntegerish(seasonal.difference.lag, lower = 0L, upper = 100000L)
  assertLogical(na.pad)
  assert(checkClass(obj, "data.frame"), checkClass(obj, "Task"))
  assertCharacter(target, any.missing = FALSE)
  if (!is.null(cols))
    assertCharacter(cols, any.missing = FALSE)

  UseMethod("createLagDiffFeatures")
}

#' @export
createLagDiffFeatures.data.frame = function(obj, target = character(0L),
  lag = 0L, difference = 0L, difference.lag = 0L,
  cols = NULL, seasonal.cols = NULL, seasonal.lag = 0L, seasonal.difference = 0L,
  seasonal.difference.lag = 0L, frequency = 1L, add_dates = "yday",
  na.pad = FALSE, return.nonlag = FALSE, grouping = NULL, add_var = FALSE,
  TTR.funcs = NULL, date.col) {

  work.cols = colnames(obj)
  if (missing(date.col)) {
    stop("Dates must be given")
  } else {
    data = as.data.table(obj)
    data[, dates := date.col]
    suppressWarnings(setkeyv(data, c(grouping, "dates")))
  }
  if (!is.null(cols)) {
    assertSubset(cols, work.cols)
    x = data[, c(cols, grouping), with = FALSE]
  } else {
    cols = work.cols
    x = data[, cols, with = FALSE]
  }
  if (!is.null(seasonal.cols)) {
    if (!(target %in% seasonal.cols))
      #seasonal.cols = c(seasonal.cols, target)
      assertSubset(seasonal.cols, work.seasonal.cols)
    x = data[, c(seasonal.cols, grouping), with = FALSE]
  } else {
    seasonal.cols = cols
  }
  cols = cols[!(cols %in% grouping)]
  seasonal.cols = seasonal.cols[!(seasonal.cols %in% grouping)]
  lag.diff.full.names = vector(mode = "character")

  if (any(lag > 0)) {
    lag.vars = cols
    lag.value = lag
    lag.names = do.call("paste", c(CJ(lag.vars, "lag", lag.value,
      sorted = FALSE), sep = "_"))
    x[, c(lag.names) := shift(.SD, lag.value), by = eval(c(grouping)), .SDcols = lag.vars]
    lag.diff.full.names = c(lag.diff.full.names, lag.names)
  }

  pad  = function(x, n) {
    len.diff = n - length(x)
    c(rep(NA, len.diff), x)
  }

  if (add_var) {
    var.lag.names = do.call("paste", c(CJ("var", target, lag.value, sorted = FALSE), sep = "_"))
    x[,(var.lag.names) :=  lapply(.SD, function(x) pad(cumsum(na.omit((get(target) - x)^2)),length(x))) , by = eval(c(grouping)), .SDcols = lag.names]
    lag.diff.full.names = c(lag.diff.full.names, var.lag.names)
  }

  if (any(difference > 0) | any(difference.lag > 0)) {

    diff.vars = vlapply(x[, c(cols), with = FALSE], is.numeric)
    diff.vars = cols[diff.vars]
    # Since the default for both is zero, which would throw an error, set the zero one to 1
    if (any(difference > 0)) {
      difference.value = difference
    } else {
      difference.value = 1
    }
    if (any(difference.lag > 0)) {
      difference.lag.value = difference.lag
    } else {
      difference.lag.value = 1
    }
    diff.table = expand.grid(difference.value, difference.lag.value)
    diff.full.names = list()[seq_len(nrow(diff.table))]
    for (i in seq_len(nrow(diff.table))) {
      diff.iter = as.numeric(diff.table[i, ])
      diff.names = do.call("paste", c(CJ(diff.vars, "diff", diff.iter[1], "lag", diff.iter[2], sorted = FALSE), sep = "_"))
      x[, c(diff.names) := lapply(.SD,
        function(xx) pad(diff(xx, lag = diff.iter[2], differences = diff.iter[1]), length(xx))),
        by = eval(c(grouping)), .SDcols = diff.vars]
      diff.full.names[[i]] = diff.names
    }
    lag.diff.full.names = c(lag.diff.full.names, unlist(diff.full.names))
  }

  if (!is.null(TTR.funcs)) {
    TTR.vars = vlapply(x[, c(cols), with = FALSE], is.numeric)
    TTR.vars = cols[TTR.vars]
    TTR.name = names(TTR.funcs)
    for (i in seq_len(length(TTR.funcs))) {
      TTR.iter = TTR.funcs[[i]]
      TTR.func.name = TTR.name[i]
      TTR.args = expand.grid(TTR.iter)
      for (j in seq_len(nrow(TTR.args))) {
        TTR.arg.iter = TTR.args[j,]
        TTR.arg.list = BBmisc::convertColsToList(TTR.arg.iter)
        TTR.var.names = paste0(TTR.vars, ".", TTR.func.name, ".", paste(TTR.arg.list, collapse = "."))
        TTR.var.names = diff.names = do.call("paste", c(CJ(TTR.vars, TTR.func.name,
          paste(TTR.arg.list, collapse = "_"), sorted = FALSE), sep = "_"))

        x[, c(TTR.var.names) :=  lapply(.SD, function(xx) {
          do.call(get(TTR.func.name),
                  unlist(list(x = list(xx), TTR.arg.list), recursive = FALSE))
          }),
          .SDcols = cols]
        lag.diff.full.names = c(lag.diff.full.names, TTR.var.names)
      }
    }
  }


  if (frequency > 1L) {
    if (any(seasonal.lag > 0)) {

      seasonal.lag.vars = seasonal.cols
      seasonal.lag.value = seasonal.lag * frequency
      seasonal.lag.levels = as.vector(vapply(seasonal.lag.value, function(levels) rep(levels, length(seasonal.lag.vars)),
        c(rep(1.0, length(seasonal.lag.vars)))))
      seasonal.lag.names = do.call("paste", c(CJ(seasonal.lag.vars, ".lag.", unique(seasonal.lag.levels), sorted = FALSE), sep = "_"))
      x[, c(seasonal.lag.names) := shift(.SD, seasonal.lag.value), by = eval(c(grouping)), .SDcols = seasonal.lag.vars]
      lag.diff.full.names = c(lag.diff.full.names, seasonal.lag.names)
    }

    if (any(seasonal.difference > 0) | any(seasonal.difference.lag > 0)) {

      diff.vars = vlapply(x[, c(seasonal.cols), with = FALSE], is.numeric)
      diff.vars = seasonal.cols[diff.vars]
      # Since the default for both is zero, which would throw an error, set the zero one to 1
      if (any(seasonal.difference > 0)) {
        seasonal.difference.value = seasonal.difference * frequency
      } else {
        seasonal.difference.value = 1
      }
      if (any(seasonal.difference.lag > 0)) {
        seasonal.difference.lag.value = seasonal.difference.lag * frequency
      } else {
        seasonal.difference.lag.value = 1
      }
      seasonal.diff.vars = vlapply(x[, c(seasonal.cols), with = FALSE], is.numeric)
      seasonal.diff.vars = seasonal.cols[seasonal.diff.vars]

      seasonal.diff.table = expand.grid(seasonal.difference.value, seasonal.difference.lag.value)
      seasonal.diff.full.names = list()[seq_len(nrow(seasonal.diff.table))]
      for (i in seq_len(nrow(seasonal.diff.table))) {
        seasonal.diff.iter = as.numeric(seasonal.diff.table[i, ])
        seasonal.diff.lag.names = as.numeric(rep(seasonal.diff.iter[2], length(seasonal.diff.vars)))
        seasonal.diff.diff.names = as.numeric(rep(seasonal.diff.iter[1], length(seasonal.diff.vars)))
        seasonal.diff.names = do.call("paste", c(CJ(seasonal.diff.vars, "diff", seasonal.diff.diff.names,
          "lag", seasonal.diff.lag.names, sorted = FALSE), sep = "_"))

        x[, c(seasonal.diff.names) := lapply(.SD,
          function(xx) pad(diff(xx, lag = seasonal.diff.iter[2], differences = seasonal.diff.iter[1]), length(xx))),
          by = eval(c(grouping)), .SDcols = seasonal.diff.vars]
        seasonal.diff.full.names[[i]] = seasonal.diff.names
      }
      lag.diff.full.names = c(lag.diff.full.names, unlist(seasonal.diff.full.names))
    }
  }

  if (!is.null(add_dates)) {
    for (date_func in add_dates) {
      x[, paste0(date_func, "_dt") := get(date_func)(date.col) - min(get(date_func)(date.col))]
      lag.diff.full.names = c(lag.diff.full.names, paste0(date_func, "_dt"))
    }
  }

  max.shift = 1:(max(lag, seasonal.lag * frequency,
    max(difference, 1) * max(difference.lag, 1),
    max(seasonal.difference * frequency), max( seasonal.difference.lag * frequency)))


  if (return.nonlag) {
    data = data[, c(setdiff(work.cols, union(cols, seasonal.cols)), "dates"), drop = FALSE, with = FALSE]
    if (ncol(data) != 0) {
      data = cbind(data, x)
    } else {
      data = x
    }
  } else {
    data_cols = unique(c(setdiff(work.cols, union(cols, seasonal.cols)), "dates", target))
    data = cbind(data[, ..data_cols], x[, c(lag.diff.full.names), with = FALSE])
  }
  if (!na.pad) {
    data = data[, .SD[-max.shift, ], by = eval(c(grouping))]
  }
  setkey(data, "dates")
  data$dates = NULL
  return(as.data.frame(data))
}

#' @export
createLagDiffFeatures.Task = function(obj, target = character(0L),
  lag = 0L, difference = 0L, difference.lag = 0L,
  cols = NULL, seasonal.cols = NULL, seasonal.lag = 0L, seasonal.difference = 0L,
  seasonal.difference.lag = 0L, frequency = 1L, add_dates = "yday",
  na.pad = FALSE, return.nonlag = FALSE, grouping = NULL,
  add_var = FALSE, TTR.funcs = NULL, date.col) {

  target = getTaskTargetNames(obj)
  data = getTaskData(obj)

  td = getTaskDesc(obj)
  target = getTaskTargetNames(obj)
  if (!is.null(td$frequency) && frequency == 1L)
    frequency = td$frequency
  # We store the original columns as we need them for forecasting
  data.original = data

  if (missing(date.col)) {
    if (is.null(td$dates)) {
      stop("Dates must be supplied")
    } else {
      date.col = td$dates
    }
  }
  data = createLagDiffFeatures( obj = data, target = target, lag = lag, difference = difference,
    difference.lag = difference.lag,
    cols = cols, seasonal.cols = seasonal.cols,
    seasonal.lag = seasonal.lag,
    seasonal.difference = seasonal.difference,
    seasonal.difference.lag = seasonal.difference.lag,
    frequency = frequency, add_dates = add_dates, na.pad = na.pad,
    return.nonlag = return.nonlag, grouping = grouping,
    add_var = add_var, TTR.funcs = TTR.funcs, date.col = date.col)

  obj = changeData(obj, data = data)

  max.shift = max(lag, seasonal.lag * frequency,
    max(difference) * max(difference.lag),
    max(seasonal.difference * frequency) * max(seasonal.difference.lag * frequency))
  data.original = data.table(data.original)
  data.original = data.original[,.SD[ (.N - max.shift):.N,], by = eval(c(grouping))]

  obj$task.desc$pre.proc$data.original = data.original
  obj$task.desc$pre.proc$par.vals = list(lag = lag, difference = difference,
    difference.lag = difference.lag,
    cols = cols, seasonal.cols = seasonal.cols, target = target,
    seasonal.lag = seasonal.lag,
    seasonal.difference = seasonal.difference,
    seasonal.difference.lag = seasonal.difference.lag,
    frequency = frequency, add_dates = add_dates, na.pad = na.pad,
    return.nonlag = return.nonlag, grouping = grouping, add_var = add_var,
    TTR.funcs = TTR.funcs, date.col = date.col)
  obj
}

