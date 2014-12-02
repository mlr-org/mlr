#' @title Visualize binary classification predictions via ROCR ROC curves.
#'
#' @description
#' Please see \code{\link[ROCR]{ROCR}} for further info regarding plots.
#'
#' @param obj [(list of) \code{\link{Prediction}} | \code{\link{BenchmarkResult}}]\cr
#'   Single prediction object, list of them, or a benchmark result.
#'   In case of a list probably produced by different learners you want to compare, then
#'   name the list with the names you want to see in the plots, probably
#'   learner shortnames or ids.
#' @param meas1 [\code{character(1)}]\cr
#'   Measure on x-axis. Note that this is a measure name from *ROCR* and not from mlr!
#'   Default is \dQuote{tpr}.
#' @param meas2 [\code{character(1)}]\cr
#'   Measure on y-axis. Note that this is a measure name from *ROCR* and not from mlr!
#'   Default is \dQuote{tpr}.
#' @param avg [\code{character(1)}]\cr
#'   How to average results from resampling.
#'   Default is \dQuote{threshold}.
#' @param task.id [\code{character(1)}]\cr
#'   Selected task in \code{\link{BenchmarkResult}} to do plots for, ignored otherwise.
#'   Default is first taks.
#' @return [\code{character(1)}]. Invisibly returns the ViperCharts URL.
#' @family roc
#' @family predict
#' @export
#' @references
#' Sluban and Lavrač - ViperCharts: Visual Performance Evaluation Platform,
#' ECML PKDD 2013, pp. 650-653, LNCS 8190, Springer, 2013.
#' @examples
#' \dontrun{
#' lrn1 = makeLearner("classif.logreg", predict.type = "prob")
#' lrn2 = makeLearner("classif.rpart", predict.type = "prob")
#' b = benchmark(list(lrn1, lrn2), pid.task)
#' z = plotROCRCurves(b, chart = "lift", browse = TRUE)
#' }
plotROCRCurves = function(obj, meas1 = "tpr", meas2 = "fpr", avg = "threshold", legpos = "bottomright", task.id = NULL) {
  # lets not check the value-names from ROCR here. they might be changed behind our back later...
  assertString(legpos)
  assertString(meas1)
  assertString(meas2)
  assertString(avg)
  UseMethod("plotROCRCurves")
}

#' @export
plotROCRCurves.Prediction = function(obj, meas1 = "tpr", meas2 = "fpr", avg = "threshold", legpos = "bottomright", task.id = NULL) {
  l = list(obj)
  names(l) = getTaskId(obj)
  plotROCRCurves.list(l, meas1, meas2, avg, legpos)
}

#' @export
plotROCRCurves.list = function(obj, meas1 = "tpr", meas2 = "fpr", avg = "threshold", legpos = "bottomright", task.id = NULL) {
  assertList(obj, "Prediction", min.len = 1L)
  k = length(obj)
  rocr.perfs = lapply(obj, function(x) {
    rp = asROCRPrediction(x)
    ROCR::performance(rp, "tpr", "fpr")
  })
  ns = names(obj)
  for (i in 1:k) {
    ROCR::plot(rocr.perfs[[i]], avg = avg, add = (i > 1L))
  }
  if (k > 1L)
    legend(x = legpos, legend = ns)
  invisible(NULL)
}

#' @export
plotROCRCurves.BenchmarkResult = function(obj, meas1 = "tpr", meas2 = "fpr", avg = "threshold", legpos = "bottomright", task.id = NULL) {
  tids = getBMRTaskIds(obj)
  if (is.null(task.id))
    task.id = tids[1L]
  else
    assertChoice(task.id, tids)
  ps = getBMRPredictions(obj, task.ids = task.id, as.df = FALSE)[[1L]]
  plotROCRCurves.list(ps, meas1, meas2, avg, legpos)
}

