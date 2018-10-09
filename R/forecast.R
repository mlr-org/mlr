#' @title  Forecasting generic
#'
#' @description Forecasts using a fitted model
#'
#' @param object
#'  A model to be used for forecasting
#'
#' @param ...
#'  Used to pass package specific parameters
#' @export
forecast = function(object, ...) {
  UseMethod("forecast")
}

#' @title Forecast a trained learner.
#'
#' @description
#' Forecast the target variable of new data using a fitted model.
#' What is stored exactly in the [Prediction] object depends
#' on the \code{predict.type} setting of the \code{\link{Learner}}.
#' If \code{predict.type} was set to \dQuote{prob} probability thresholding
#' can be done calling the \code{\link{setThreshold}} function on the
#' prediction object.
#'
#' The row names of the input \code{task} or \code{newdata} are preserved in the output.
#'
#' @param object [WrappedModel]\cr
#'   Wrapped model, result of `train`.
#' @param task [Task]\cr
#'   The task. This is only used for fcregr and mfcregr, in which it's used to gather date information
#' @param newdata [data.frame]\cr
#'   Optional: A data frame of external regressors which must have the same number of rows as your forecast length h
#' @param h (`integer(1)`)
#'   An integer specifying the forecast horizon
#' @param ... [any]\cr
#'   Currently ignored.
#' @return [Prediction].
#' @family predict
#' @export
#' @examples
#' # train and forecast
#' dat  = arima.sim(model = list(ar = c(.8,.1), ma = c(.4), order = c(2,0,1)), n = 300)
#' jump = data.frame(jump = ifelse(diff(dat) > .5, "up","down"))
#' jump.times = as.POSIXct("1992-01-14") + 1:299
#'
#' classif.task = makeClassifTask(data = jump,target = "jump")
#' classif.task = createLagDiffFeatures(classif.task, lag = 1L:15L,
#'                                      na.pad = FALSE, date.col = jump.times)
#' classif.learn = makeLearner("classif.ada")
#' classif.train = train(classif.learn,classif.task)
#' forecast(classif.train, h = 10)
#'
#' # predict probabiliies instead of class labels
#' lrn = makeLearner("classif.lda", predict.type = "prob")
#' model = train(lrn, classif.task)
#' f = forecast(model, h = 10)
#' print(f)
#' getPredictionProbabilities(f)
forecast.WrappedModel = function(object, newdata = NULL, task, h = 10, ...) {
  model = object
  learner = model$learner
  td = model$task.desc
  assertIntegerish(h, lower = 1)
  assertClass(model, classes = "WrappedModel")
  if (any(c("fcregr", "mfcregr") %in% model$learner$type ))
    if (!missing(newdata))
      return(predict(object, newdata = newdata))
  else
    return(predict(object, task = task))

  if (!missing(task))
    stop("Tasks are only accepted for fcregr and mfcregr tasks.
      forecast is only used after you have already trained your learner so only newdata is accepted")
  if (is.null(td$pre.proc))
    stop("Forecasting with a learner requires a createLagDiffFeatures preproc.")

  if (!is.null(newdata)) {
    if (class(newdata)[1] != "data.frame") {
      warningf("Provided data for prediction is not a pure data.frame but from class %s, hence it will be converted.",  class(newdata)[1])
      newdata = as.data.frame(newdata)
    }
    assertDataFrame(newdata)
    t.col = match(td$target, colnames(newdata))
    #    if (nrow(newdata) != h)
    #      stop("The new data supplied must be the length of your forecast")
  } else {
    t.col = NA
  }
  # get truth and drop target col, if target in newdata
  if (!all(is.na(t.col))) {
    if (length(t.col) > 1L && anyMissing(t.col))
      stop("Some but not all target columns found in data")
    truth = newdata[, t.col, drop = TRUE]
    if (is.list(truth))
      truth = data.frame(truth)
    newdata = newdata[, -t.col, drop = FALSE]
  } else {
    truth = NULL
  }


  proc.vals = td$pre.proc$par.vals
  max.lag = max(c(proc.vals$lag, proc.vals$difference.lag,
    proc.vals$seasonal.lag * proc.vals$frequency,
    proc.vals$seasonal.difference.lag * proc.vals$frequency))

  data = td$pre.proc$data.original
  if (is.null(truth)) {
    row.dates = as.POSIXct(proc.vals$date.col)
    diff.time = difftime(row.dates[2], row.dates[1], units = "auto")
    start = row.dates[length(row.dates)] + diff.time
    end = start + diff.time * h
    if (!is.null(proc.vals$grouping)) {
      if (is.null(newdata)) {
        group.names = unique(data[[proc.vals$grouping]])
      } else {
        group.names = unique(newdata[[proc.vals$grouping]])
      }
      row.names = rep(seq.POSIXt(start, end - 1, by = diff.time), each = length(group.names))
    } else {
      row.names = seq.POSIXt(start, end - 1, by = diff.time)
    }

  } else {
    row.names = row.names(truth)
  }
  #data = data[max.lag:length(data),1,drop = FALSE]

  error = NA_character_
  # was there an error in building the model? --> return NAs
  if (isFailureModel(model)) {
    p = predictFailureModel(model, newdata)
    time.predict = NA_real_
  } else {
    pars = list(
      .data = data,
      .newdata = newdata,
      .proc.vals = proc.vals,
      .h = h,
      .td = td,
      .model = model
    )
    debug.seed = getMlrOption("debug.seed", NULL)
    if (!is.null(debug.seed))
      set.seed(debug.seed)
    opts = getLearnerOptions(learner, c("show.learner.output", "on.learner.error", "on.learner.warning"))
    fun1 = if (opts$show.learner.output) identity else capture.output
    fun2 = if (opts$on.learner.error == "stop") identity else function(x) try(x, silent = TRUE)
    fun3 = if (opts$on.learner.error == "stop" || !opts$on.error.dump) identity else function(x) {
      withCallingHandlers(x, error = function(c) utils::dump.frames())
    }
    if (opts$on.learner.warning == "quiet") {
      old.warn.opt = getOption("warn")
      on.exit(options(warn = old.warn.opt))
      options(warn = -1L)
    }
    time.predict = measureTime(fun1({p = fun2(fun3(do.call(makeForecast, pars)))}))
  }

  ids = NULL
  if (!is.null(p$grouping)) {
    row.names = unique(do.call("paste", c(CJ(p$grouping, "lag", row.names,
      sorted = FALSE), sep = ".")))
    p$grouping = NULL
  }
  makePrediction(task.desc = td, row.names = row.names, id = ids, truth = truth,
    predict.type = learner$predict.type,
    predict.threshold = learner$predict.threshold,
    y = p, time = time.predict, error = error)
}

.getForecastResponse = function(pred) {
  if (pred$predict.type == "prob") {
    colnames(pred$data) = stri_replace_all_regex(colnames(pred$data), "prob", "")
    colnames(pred$data) = stri_replace_all_regex(colnames(pred$data), "[.]", "")
  }
  return(pred$data)
}

makeForecast = function(.data, .newdata, .proc.vals, .h, .td, .model, ...) {
  forecasts = list()[1:I(.h)]
  if (!is.null(.proc.vals$grouping)) {
    if (!is.null(.newdata[, .proc.vals$grouping])) {
      .data = .data[get(.proc.vals$grouping) %in% unique(.newdata[, c(.proc.vals$grouping),])]
    }
    group.data = unique(.data[,.proc.vals$grouping, with = FALSE])
  }
  # get lag structure
  lagdiff.func = function(...) {
    createLagDiffFeatures(obj = .data, ...)
  }

  for (i in seq_len(.h)) {
    times = .data[, .SD[,as.POSIXct("1992-01-14") + 1:.N], by = eval(.proc.vals$grouping)]
    times = times[, c(V1), drop = TRUE]
    .proc.vals$date.col = times
    if (!is.null(.newdata) & i != 1) {
      .newdata = data.table(.newdata)
      last_rows = .data[, .I[.N], by = eval(.proc.vals$grouping)]$V1
      new_cols = setdiff(colnames(.newdata), .proc.vals$grouping)
      new_row = .newdata[, .SD[i - 1, ], by = eval(.proc.vals$grouping)][, c(new_cols), with = FALSE]
      .data[last_rows, c(colnames(.newdata)) := new_row]
    }
    data.lag = do.call(lagdiff.func, .proc.vals)
    data.step = data.table(data.lag)
    data.step = data.step[, .SD[.N,], by = eval(.proc.vals$grouping)]
    data.step = data.step[, -c(.td$target), with = FALSE]
    # predict
    pred = predict(.model, newdata = as.data.frame(data.step))
    forcast_df = .getForecastResponse(pred)
    if (!is.null(group.data)) {
      forcast_df[, "grouping"] = group.data
    }
    forecasts[[i]] = forcast_df
    if (!is.null(.proc.vals$grouping)) {
      if (is.null(.proc.vals$cols)) {
        cols = colnames(.data)
      } else {
        cols = .proc.vals$cols
      }
      .datalist = list(.data[,c(union(union(cols, .proc.vals$target), .proc.vals$grouping)),
                             with = FALSE], group.data)
      .data = rbindlist(l = .datalist, fill = TRUE)
      setkeyv(.data, .proc.vals$grouping)
    } else if (!is.null(.proc.vals$cols)) {
      .data = suppressWarnings(do.call(rbind, list(.data,.data[1E+99,])))
    } else {
      .data = rbindlist(list(.data[, c(.proc.vals$target), with = FALSE], data.table(NA)), use.names = FALSE)
    }
    .data[is.na(get(.proc.vals$target)), c(.proc.vals$target) := getPredictionResponse(pred)]
  }

  p = do.call(rbind, forecasts)
  p$truth = NULL
  p
  p
}
