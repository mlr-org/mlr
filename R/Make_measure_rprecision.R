#' @title Creates the measure R-precision or top p-accuracy
#'
#' @description
#' Creates a measure for anomaly detection with external ground truth (known labels).
#' R-precision or top p-accuracy measures the relativ number of true predicted anomalies
#' in the top p ranks of the test set to the number of anomalies on the entire test set.
#'
#' @param id [\code{character(1)}]\cr
#'   Name of measure.
#'   Default is \dQuote{costs}.
#' @param p [\code{numeric}] \cr
#'   top p rank, with p element of [0, number of anomalies in data]
#' @param adjusted [\code{logical}] \cr
#'   adjusted for chances. Recommend to compare different data sets. Default TRUE.
#' @return [\code{numeric(1)}]
#'  R precision/ top p-accuracy
#' @references Campos, Guilherme O., et al. "On the evaluation of unsupervised outlier detection: measures, datasets, and an empirical study." Data Mining and Knowledge Discovery 30.4 (2016): 891-927.
#' @inheritParams makeMeasure
#' @template ret_measure
#' @export
#' @family performance.
#' @examples
#' # creates an R-precision measure which calculates the R-precision for the top 10 (=p) ranks
#' rprecision = makeRPrecisionMeasure(id = "RPrecision", minimize = FALSE, best = 0, worst = NULL, p = 5, adjusted = TRUE)
#'
#' data = getTaskData(oneclass2d.task)
#' inds.split = chunk(seq_len(nrow(data)), shuffle = TRUE, props = c(0.6, 0.4))
#' train.inds = inds.split[[1]]
#' test.inds = inds.split[[2]]
#' lrn = makeLearner("oneclass.svm", predict.type = "prob")
#' mod = train(lrn, oneclass2d.task, subset = train.inds)
#' pred = predict(mod, oneclass2d.task, subset = test.inds)
#'
#' # calculate performance for prediction object, pass data of features used for
#' # prediction as feats in performance
#' performance(pred = pred, measures = list(rprecision), model = mod, task = task)


makeRPrecisionMeasure = function(id = "RPrecision", minimize = FALSE, best = 1, worst = 0, name = id, note = "", p, adjusted = TRUE) {

  assertString(id)
  assertFlag(minimize)
  assertLogical(adjusted)
  assertString(name)
  assertString(note)

  makeMeasure(id = id, minimize = minimize, extra.args = list(p, adjusted),
    properties = c("oneclass", "classif", "req.pred", "req.truth"),
    best = best,
    worst = worst,
    fun = function(task, model, pred, feats, extra.args) {
      p = extra.args[[1]]
      adjusted = extra.args[[2]]
      browser()
      n.anomaly = sum(pred$data$truth == pred$task.desc$positive)
      if (p > n.anomaly) stopf("p has to be in the intervall [0, number of anomalies in data]")

      scores = pred$data$prob.TRUE
      rank = order(scores)
      ind.true = which(pred$data$truth == pred$task.desc$positive)
      if (length(ind.true) == 0) {
        warning("There are no anomalies in the data set. RPrecision is NA")
        rprecision = NA
      } else {
        rprecision = sum(rank[ind.true] <= p)/p
      }

      if (adjusted == TRUE) {
        expected.index = n.anomaly/nrow(pred$data)
        rprecision = (rprecision - expected.index)/(1- expected.index)
      }
      rprecision
    },
    name = name,
    note = note
  )
}

