#' @title Visualize binary classification predictions via ViperCharts system.
#'
#' @description
#' This includes ROC, lift charts, cost curves, and so on.
#' Please got to <http://viper.ijs.si> for further info.
#'
#' For resampled learners, the predictions from different iterations are combined into one.
#' That is, for example for cross-validation, the predictions appear on
#' a single line even though they were made by different models. There is currently
#' no facility to separate the predictions for different resampling iterations.
#'
#' @template arg_plotroc_obj
#' @param chart (`character(1)`)\cr
#'   First chart to display in focus in browser.
#'   All other charts can be displayed by clicking on the browser page menu.
#'   Default is \dQuote{rocc}.
#' @param browse (`logical(1)`)\cr
#'   Open ViperCharts plot in web browser? If not you simple get the URL returned.
#'   Calls [browseURL].
#'   Default is `TRUE`.
#' @param auth.key (`character(1)`)\cr
#'   API key to use for call to Viper charts website.
#'   Only required if you want the chart to be private.
#'   Default is `NULL`.
#' @param task.id (`character(1)`)\cr
#'   Selected task in [BenchmarkResult] to do plots for, ignored otherwise.
#'   Default is first task.
#' @return (`character(1)`). Invisibly returns the ViperCharts URL.
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
#' z = plotViperCharts(b, chart = "lift", browse = TRUE)
#' }
plotViperCharts = function(obj, chart = "rocc", browse = TRUE, auth.key = NULL, task.id = NULL) {
  UseMethod("plotViperCharts")
}

#' @export
plotViperCharts.Prediction = function(obj, chart = "rocc", browse = TRUE, auth.key = NULL, task.id = NULL) {
  td = getPredictionTaskDesc(obj)
  l = namedList(names = getTaskId(td), init = obj)
  plotViperCharts.list(l, chart, browse, auth.key)
}

#' @export
plotViperCharts.ResampleResult = function(obj, chart = "rocc", browse = TRUE, auth.key = NULL, task.id = NULL) {
  l = namedList(names = getTaskId(obj), init = obj)
  plotViperCharts.list(l, chart, browse, auth.key)
}

#' @export
plotViperCharts.list = function(obj, chart = "rocc", browse = TRUE, auth.key = NULL, task.id = NULL) {

  assertList(obj, c("Prediction", "ResampleResult"), min.len = 1L)
  assertChoice(chart, c("prs", "rocs", "prc", "lift", "rocc", "roch", "ROC", "cost",
    "ratedriven", "kendall", "column"))
  requirePackages(c("rjson", "RCurl"), why = "plotViperChart", default.method = "load")

  inp = mapply(function(p, s) {
    if (inherits(p, "ResampleResult")) {
      s = p$learner.id
      p = getRRPredictions(p)
    }
    checkPrediction(p, task.type = "classif", binary = TRUE, predict.type = "prob", check.truth = TRUE)
    a = p$data$truth
    a = as.numeric(a == p$task.desc$positive)
    list(name = s, actual = a, predicted = getPredictionProbabilities(p))
  }, obj, names(obj), SIMPLIFY = FALSE, USE.NAMES = FALSE)
  inp = list(
    chart = chart, data = inp, embedded = "false"
  )
  url = "http://viper.ijs.si/api/"
  headers = list("Accept" = "application/json", "Content-Type" = "application/json")
  if (!is.null(auth.key))
    headers["AUTH-KEY"] = auth.key
  resp = RCurl::postForm(url, .opts = list(postfields = rjson::toJSON(inp), httpheader = headers))
  resp = rjson::fromJSON(resp)
  if (resp$url == "")
    stopf("ViperCharts error: %s", resp$msg)
  if (browse)
    browseURL(resp$url)
  invisible(resp$url)
}

#' @export
plotViperCharts.BenchmarkResult = function(obj, chart = "rocc", browse = TRUE, auth.key = NULL, task.id = NULL) {
  tids = getBMRTaskIds(obj)
  if (is.null(task.id))
    task.id = tids[1L]
  else
    assertChoice(task.id, tids)
  ps = getBMRPredictions(obj, task.ids = task.id, as.df = FALSE)[[1L]]
  plotViperCharts.list(ps, chart, browse, auth.key)
}
