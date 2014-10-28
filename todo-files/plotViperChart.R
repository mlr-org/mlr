# FIXME: send simple R snippet to borut

# document averaging / cv issue. is this hanlded by VC?
# support benchmark results
# cite paper

#' @title Visualize binary classification predictions via ViperCharts system.
#'
#' @description
#' This includes ROC, lift charts, cost curves, and so on.
#' Please got to \url{http://viper.ijs.si} for further info.
#'
#' @param obj [(list of) \code{\link{Prediction}}}]\cr
#'   List of prediction objects or a single object for the plot,
#'   in case of a list probably produced by different learners you want to compare.
#'   Name the list with the names you want to see in the plots, probably
#'   learner shortnames or ids.
#' @param chart [\code{character(1)}]\cr
#'   First chart to display in focus in browser.
#'   All curves are precomputed by ViperCharts, simply select them by clicking.
#'   Default is \dQuote{rocc}.
#' @param browser [\code{logical(1)}]\cr
#'   Open ViperCharts plot in web browser? If not you simple get the URL returned.
#'   Calls \code{\link{browseURL}}.
#'   Default is \code{TRUE}.
#' @return [\code{character(1)}]. Invisibly returns the ViperCharts URL.
#' @family ???
#' @export
#' @examples
plotViperCharts = function(obj, chart = "rocc", browser = TRUE) {
  UseMethod("plotViperCharts")
}

plotViperCharts.Prediction = function(obj, chart = "rocc", browser = TRUE) {
  plotViperCharts.list(list(obj), chart = chart, browser = browser)
}

plotViperCharts.list = function(obj, chart = "rocc", browser = TRUE) {
  assertChoice(chart, c("prs", "rocs", "prc", "lift", "rocc", "roch", "ROC", "cost",
    "ratedriven", "kendall", "column"))
  assertFlag(browser)

  requirePackages("rjson", why = "plotViperChart")
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
  resp = postForm(url, .opts = list(postfields = toJSON(inp), httpheader = headers))
  resp = fromJSON(resp)
  if (resp$url == "")
    stopf("ViperCharts error: %s", resp$msg)
  # lets get the charts with the more and and fancier buttons
  resp$url = sub("api/curve", "chart", resp$url)
  if (browser)
    browseURL(resp$url)
  invisible(url)
}

plotViperCharts.BenchmarkResult = function(obj, task.id = NULL, chart = "rocc", browser = TRUE) {
  tids = getBMRTaskIds(obj)
  if (is.null(task.id))
    task.id = tids[1L]
  else
    assertChoice(task.id, tids)
  ps = getBMRPredictions(obj, task.ids = task.id, as.df = FALSE)[[1L]]
  plotViperCharts.list(ps, chart = chart, browser = browser)
}
