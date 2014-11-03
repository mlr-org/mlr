# FIXME:
# document averaging / cv issue. is this hanlded by VC?
# cite paper

#' @title Visualize binary classification predictions via ViperCharts system.
#'
#' @description
#' This includes ROC, lift charts, cost curves, and so on.
#' Please got to \url{http://viper.ijs.si} for further info.
#'
#' @param obj [(list of) \code{\link{Prediction}} | \code{\link{BenchmarkResult}}]\cr
#'   Single prediction object, list of them, or a benchmark result.
#'   In case of a list probably produced by different learners you want to compare, then
#'   name the list with the names you want to see in the plots, probably
#'   learner shortnames or ids.
#' @param chart [\code{character(1)}]\cr
#'   First chart to display in focus in browser for non-embedded view,
#'   or displayed chart for embedded mode.
#'   Default is \dQuote{rocc}.
#' @param browse [\code{logical(1)}]\cr
#'   Open ViperCharts plot in web browser? If not you simple get the URL returned.
#'   Calls \code{\link{browseURL}}.
#'   Default is \code{TRUE}.
#' @return [\code{character(1)}]. Invisibly returns the ViperCharts URL.
#' @family roc
#' @family predict
#' @export
#' @examples
#' \dontrun{
#' lrn1 = makeLearner("classif.logreg", predict.type = "prob")
#' lrn2 = makeLearner("classif.rpart", predict.type = "prob")
#' b = benchmark(list(lrn1, lrn2), pid.task)
#' z = plotViperCharts(b, chart = "lift", browse = TRUE)
#' }
plotViperCharts = function(obj, chart = "rocc", browse = TRUE) {
  UseMethod("plotViperCharts")
}

plotViperCharts.Prediction = function(obj, chart = "rocc", browse = TRUE) {
  plotViperCharts.list(list(obj), chart, browse)
}

plotViperCharts.list = function(obj, chart = "rocc", browse = TRUE) {

  assertChoice(chart, c("prs", "rocs", "prc", "lift", "rocc", "roch", "ROC", "cost",
    "ratedriven", "kendall", "column"))

  requirePackages(c("rjson", "RCurl"), why = "plotViperChart")
  inp = mapply(function(p, s) {
    checkPrediction(p, "classif", "prob")
    a = p$data$truth
    a = as.numeric(a == p$task.desc$positive)
    list(name = s, actual = a, predicted = getProbabilities(p))
  }, obj, names(obj), SIMPLIFY = FALSE, USE.NAMES = FALSE)
  inp = list(
    chart = chart, data = inp
  )
  url = "http://viper.ijs.si/api/"
  headers = list('AUTH-KEY' = "soanclCNdnLDcnlNc", 'Accept' = 'application/json',
    'Content-Type' = 'application/json')
  resp = RCurl::postForm(url, .opts = list(postfields = rjson::toJSON(inp), httpheader = headers))
  resp = rjson::fromJSON(resp)
  if (resp$url == "")
    stopf("ViperCharts error: %s", resp$msg)
  if (browse)
    browseURL(resp$url)
  invisible(url)
}

plotViperCharts.BenchmarkResult = function(obj, task.id = NULL, chart = "rocc", browse = TRUE) {
  tids = getBMRTaskIds(obj)
  if (is.null(task.id))
    task.id = tids[1L]
  else
    assertChoice(task.id, tids)
  ps = getBMRPredictions(obj, task.ids = task.id, as.df = FALSE)[[1L]]
  plotViperCharts.list(ps, chart, browse)
}
