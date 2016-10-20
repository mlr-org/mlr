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
    p.split = subset(pred$data, set == s)
    p.split = split(p.split, as.factor(p.split$iter))
    # create prediction object for each resample iteration
    p.split = lapply(p.split, function (p) {
      # get predictions based on predict.type
      if (predict.type == "prob") {
        y = p[,grepl("^prob[.]", colnames(p))]
        # we need to remove the "prob." part in the colnames, otherwise
        # makePrediction thinks that the factor starts with "prob."
        colnames(y) =  gsub("^prob[.]", "", colnames(y))
      } else {
        y = p$response
      }
      makePrediction(task.desc, id = p$id,
        truth = p$truth, y = y, row.names = p$id,
        predict.type = predict.type, time = NA, ...)
    })
    # add time info afterwards
    for(i in 1:length(p.split)) p.split[[i]]$time = time[i]
    return(p.split)
  })
  ret = setNames(prediction, set)
  if (is.null(ret$train)) ret = append(ret, list(train = NULL))
  if (is.null(ret$test)) ret = append(ret, list(test = NULL))
  return(ret[c("train", "test")])
}
