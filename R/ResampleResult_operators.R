#' @title Get predictions from resample results.
#'
#' @description
#' Very simple getter.
#'
#' @param res [\code{ResampleResult}]\cr
#'   The result of \code{\link{resample}} run with \code{keep.pred = TRUE}.
#' @return [\code{ResamplePrediction}].
#' @export
#' @family resample
getRRPredictions = function(res) {
  if (is.null(res$pred))
    stopf("The 'pred' slot is empty because the ResampleResult was generated with keep.pred = FALSE.")
  else
    res$pred
}

#' @title Get task description from resample results.
#'
#' @description
#' Get a summarizing task description.
#'
#' @param res [\code{ResampleResult}]\cr
#'   The result of \code{\link{resample}}.
#' @return [\code{TaskDesc}].
#' @export
#' @family resample
getRRTaskDescription = function(res) {
  res$task.desc
}

#' @title Get list of predictions for train and test set of each single resample iteration.
#'
#' @description
#' This function creates a list with two slots \code{train} and \code{test} where
#' each slot is again a list of \code{\link{Prediction}} objects for each single
#' resample iteration.
#' In case that \code{predict = "train"} was used for the resample description
#' (see \code{\link{makeResampleDesc}}), the slot \code{test} will be \code{NULL}
#' and in case that \code{predict = "test"} was used, the slot \code{train} will be
#' \code{NULL}.
#'
#' @param res [\code{ResampleResult}]\cr
#'   The result of \code{\link{resample}} run with \code{keep.pred = TRUE}.
#' @param ... [any]\cr
#'   Further options passed to \code{\link{makePrediction}}.
#' @return [list].
#' @export
#' @family resample
getRRPredictionList = function(res, ...) {
  assertClass(res, "ResampleResult")
  # We need to force keep.pred = TRUE (will be checked in getRRPredictions)
  pred = getRRPredictions(res)
  predict.type = pred$predict.type
  time = pred$time
  task.desc = getRRTaskDescription(res)

  # split by train and test set
  set = levels(pred$data$set)

  # get prediction objects for train and test set
  prediction = lapply(set, function(s) {
    # split by resample iterations
    p.split = pred$data[pred$data$set == s,, drop = FALSE]
    p.split = split(p.split, as.factor(p.split$iter))
    # create prediction object for each resample iteration
    p.split = lapply(p.split, function (p) {
      # get predictions based on predict.type
      if (predict.type == "prob") {
        y = p[, stri_startswith_fixed(colnames(p), "prob."), drop = FALSE]
        # we need to remove the "prob." part in the colnames, otherwise
        # makePrediction thinks that the factor starts with "prob."
        colnames(y) = stri_replace_first_fixed(colnames(y), "prob.", replacement =  "")
      } else {
        y = p$response
      }
      makePrediction(task.desc, id = p$id,
        truth = p$truth, y = y, row.names = p$id,
        predict.type = predict.type, time = NA_real_, ...)
    })
    # add time info afterwards
    for(i in seq_along(p.split))
      p.split[[i]]$time = time[i]
    return(p.split)
  })

  ret = setNames(prediction, set)
  if (is.null(ret$train)) ret = append(ret, list(train = NULL))
  if (is.null(ret$test)) ret = append(ret, list(test = NULL))
  return(ret[c("train", "test")])
}

#' @title Compute new measures for existing ResampleResult
#' @description
#'  Adds new measures to an existing \code{ResampleResult}.
#' @param res [\code{ResampleResult}]\cr
#'   The result of \code{\link{resample}} run with \code{keep.pred = TRUE}.
#' @template arg_measures
#' @return [\code{\link{ResampleResult}}].
#' @export
#' @family resample
addRRMeasure = function(res, measures) {
  assertClass(res, "ResampleResult")
  if (inherits(measures, "Measure")) measures = list(measures)

  # check if measures are missing in ResampleResult object
  measures.id = vcapply(measures, function(x) x$id)
  missing.measures = setdiff(measures.id, colnames(res$measures.test))

  # if there are missing measures
  if (length(missing.measures) != 0) {
    # get list of prediction objects per iteration from resample result
    pred = getRRPredictionList(res)

    # recompute missing performance for train and/or test set
    set = names(pred)[!vlapply(pred, is.null)]
    perf = setNames(lapply(set, function(s) {
      as.data.frame(do.call("rbind", lapply(pred[[s]], function(p) {
        ret = performance(p, measures)
        matrix(ret, ncol = length(measures), dimnames = list(NULL, names(ret)))
      })))
    }), set)

    # add missing measures to resample result
    if (is.null(perf$train))
      res$measures.train[, missing.measures] = NA else
        res$measures.train = cbind(res$measures.train, perf$train[, missing.measures, drop = FALSE])
    if (is.null(perf$test))
      res$measures.test[, missing.measures] = NA else
        res$measures.test = cbind(res$measures.test, perf$test[, missing.measures, drop = FALSE])
    aggr = vnapply(measures[measures.id %in% missing.measures], function(m) {
      m$aggr$fun(task = NULL,
        perf.test = res$measures.test[, m$id],
        perf.train = res$measures.train[, m$id],
        measure = m,
        pred = getRRPredictions(res),
        group = res$pred$instance$group)
    })
    names(aggr) = vcapply(measures[measures.id %in% missing.measures], measureAggrName)
    res$aggr = c(res$aggr, aggr)
  }
  return(res)
}
