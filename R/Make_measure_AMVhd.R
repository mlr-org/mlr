#' @title Creates the measure Area under Mass-Volume Curve (AMV) for Anomaly detection (oneclass) for Dimension greater than eight by using feature subsampling
#'
#' @description
#' Creates a measure for oneclass classification for dimension greater than eight. The measure will make B feature subsamples, and in every subsample it computes the
#' Area under the Mass-Volume Curve via Monte-Carlo approximation
#' of the diagonal. It uses the trapezoidal rule as implemented in
#' package \code{caTools} for integration. The implementation is based on the
#' python implementation of the inner loop: \link{https://github.com/albertcthomas/anomaly_tuning}.
#' Differences are the type of quantile used, as the python default is not
#' available in R.
#'
#' @param id [\code{character(1)}]\cr
#'   Name of measure.
#'   Default is \dQuote{costs}.
#' @param alphas [\code{numeric}] \cr
#'   Numeric vector of alphas from 0 to 1, representing the computed quantiles.
#'   Default: 0.9 to 0.99 by 0.01 steps as we are interested in the performance
#'   of the scoring function in the the low density regions.
#' @param n.sim [\code{numeric(1)}] \cr
#'   Number of Monte-Carlo Samples, Default: 10^4.
#' @param n.feat.subsampling [\code{numeric(1)}] \cr
#'   Number of features in the subsamples, Default: 3.
#' @param n.draw.feat.subsampling [\code{numeric(1)}] \cr
#'   Number of feature subsamples B, Default: 3.
#' @return [\code{numeric(1)}]
#'   Area under Mass-Volume Curve (AMV).
#' @references Thomas, A. et al. Learning Hyperparameters for Unsupervised Anomaly Detection,
#' ICML Anomaly Detection Workshop 2016
#' @inheritParams makeMeasure
#' @template ret_measure
#' @export
#' @family performance.
#'
makeAMVhdMeasure = function(id = "AMVhd", minimize = TRUE, alphas = c(0.9, 0.99), n.alpha = 50, n.sim = 10e4, best = 0, worst = NULL, name = id, note = "") {

  assertString(id)
  assertFlag(minimize)
  assertNumeric(alphas, lower = 0, upper = 1)
  assertCount(n.alpha)
  assertCount(n.sim)
  assertString(name)
  assertString(note)

  measureAMV = makeAMVMeasure(id = "AMV", minimize = minimize, alphas = alphas, n.alpha = n.alpha, n.sim = n.sim, best = best, worst = worst, name = id)

  makeMeasure(id = id, minimize = minimize, extra.args = list(measureAMV),
    properties = c("oneclass", "req.task", "req.model", "req.pred", "predtype.prob", "req.feats"),
    best = best, worst = worst,
    fun = function(task, model, pred, feats, extra.args) {
      n.feat = getTaskNFeats(task)
      feats = getTaskData(task, target.extra = TRUE)$data
      measureAMV = extra.args[[1]]

      # get the prediction of submodels, which has sampled features
      subpred = attr(pred, "AMVhdSubpredict")

      # calculate amv for each submodel
      amv.k = lapply(subpred, function(sub.pred) {
        performance(sub.pred, measures = list(measureAMV), task, model, feats)
      })

      # Return area under mass-volume curve
      amv = mean(unlist(amv.k))
    },
    name = name,
    note = note
  )
}
