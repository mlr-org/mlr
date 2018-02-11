#' @title Creates a precision  measure: R-Precision, Precision at p (P@p), Average Precision (AP)
#'
#' @description
#' Creates a measure for anomaly detection with external ground truth (known labels).
#'
#' R-precision/Top-p Accuracy:  Measures the relativ number of true predicted anomalies
#' in the top p ranks of the test set (= t) to the number of anomalies (=p) on
#' the entire test set: t/p.
#'
#' Precision at p (P@p): Measures the relativ number of true predicted anomalies
#' in the top p ranks of the test set to the number of considered ranks p, in other
#' words it measures the proportion of correct results in the top p ranks.
#' If p equals the number of anomalies in the data the P@p is called the R-Precision.
#'
#' Avergae Precision (AP): Averages over P@p with $p$ in $\{1,2,..., number of anomalies\}$
#'
#' p usually lies in \{1,2,..., number of anomalies\}, but p can also be greater than the number of anomalies.
#' If the performance is 0, that means within the top ranks, no anomalies were detected.
#' If the performance is NA, that means no anomalies are in the data set.
#'
#' @param id [\code{character(1)}]\cr
#'   Name of measure.
#'   Default is \dQuote{Precision}.
#' @param p [\code{numeric}] \cr
#'   top p rank, with p element of \{1, 2, ..., number of anomalies in data\}
#' @param type [\code{character(1)}] \cr
#'   Set a measure type: rprecision, precisionatp, averageprecision
#'   Default is \dQuote{averageprecision}
#' @param adjusted [\code{logical}] \cr
#'   adjusted for chances. Recommend to compare different data sets. Default TRUE.
#'   (index - expected index) / (1- expected index)
#' @return [\code{numeric(1)}]
#'  Return a numeric value of R-Precision, Precision at p (P@p) or Average Precision (AP)
#' @references Campos, Guilherme O., et al. "On the evaluation of unsupervised outlier detection: measures, datasets, and an empirical study." Data Mining and Knowledge Discovery 30.4 (2016): 891-927.
#' @inheritParams makeMeasure
#' @template ret_measure
#' @export
#' @family performance.
#' @examples
#' # create an R-precision measure which calculates the relative number of true predicted
#' # anomalies in the top p ranks of the test set to the number of anomalies on the entire test set
#' rprecision = makePrecisionMeasure(id = "RPrecision", minimize = FALSE,
#' best = 0, worst = NULL, type = "rprecision", adjusted = FALSE)
#'
#' # create an P@5 measure which calculates the precision for the top 5 (=p) ranks
#' precisionat5 = makePrecisionMeasure(id = "Precisionat5", minimize = FALSE,
#' best = 0, worst = NULL, p = 5, type = "precisionatp", adjusted = FALSE)
#'
#' # create an average precision measure which calculates the average precision
#' # for all ranks of possible choices of p in \{1, 2, ..., number of anomalies\}.
#' avgprecision = makePrecisionMeasure(id = "AvgPrecision", minimize = FALSE,
#' best = 0, worst = NULL, type = "avgprecision", adjusted = FALSE)
#'
#' lrn = makeLearner("oneclass.svm", predict.type = "prob", nu = 0.05)
#' mod = train(lrn, oneclass2d.task)
#' pred = predict(mod, task = oneclass2d.task)
#'
#' # calculate performance for prediction object
#' performance(pred = pred, measures = list(rprecision, precisionat5,
#' avgprecision), model = mod, task = task)

makePrecisionMeasure = function(id = "Precision", minimize = FALSE, best = 1, worst = 0, name = id, note = "", type = c("avgprecision", "rprecision", "precisionatp"), p = NULL, adjusted = TRUE) {

  assertString(id)
  assertFlag(minimize)
  assertNumeric(p, null.ok = TRUE)
  assertString(type)
  assertLogical(adjusted)
  assertString(name)
  assertString(note)

  makeMeasure(id = id, minimize = minimize, extra.args = list(type, p, adjusted),
    properties = c("oneclass", "classif", "req.pred", "req.truth"),
    best = best,
    worst = worst,
    fun = function(task, model, pred, feats, extra.args) {
      type = extra.args[[1]]
      p = extra.args[[2]]
      adjusted = extra.args[[3]]

      n.anomaly = sum(pred$data$truth == pred$task.desc$positive)
      scores = pred$data[, 3]
      rank = order(scores, decreasing = TRUE)
      ind.true = which(pred$data$truth == pred$task.desc$positive)
      if (n.anomaly == 0) {
        warning("There are no anomalies in the data set. Measure is NA.")
        precision = NA
      } else if (type == "precisionatp") {
        if (p > n.anomaly | p < 1)
          warning("p should lie in {1, 2, ..., number of anomalies}")
        precision = sum(rank[ind.true] <= p) / p
      } else if (type == "rprecision") {
        precision = sum(rank[ind.true] <= n.anomaly) / n.anomaly
      } else if (type == "avgprecision") {
        p = 1:n.anomaly
        precision = mean(unlist(lapply(p, FUN = function(x) {sum(rank[ind.true] <= x) / x})))
      }

      if (adjusted == TRUE) {
        expected.index = n.anomaly / nrow(pred$data)
        precision = max(0, (precision - expected.index) / (1 - expected.index))
      }
      precision
    },
    name = name,
    note = note
  )
}

