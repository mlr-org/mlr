#' @title Creates the measure Area under Mass-Volume Curve (AMV) for Anomaly detection (oneclass) for data dimension less than 8
#'
#' @description
#' Creates a measure for oneclass classification. The measure computes the
#' Area under the Mass-Volume Curve via Monte-Carlo approximation
#' of the diagonal. It uses the trapezoidal rule as implemented in
#' package \code{caTools} for integration. As AMV is based in a Monte-Carlo approximation
#' the curse of dimensionality applies for data with dimension greater than eight.
#' The implementation is based on the
#' python implementation: \link{https://github.com/albertcthomas/anomaly_tuning}.
#' Differences are the type of quantile used, as the python default is not
#' available in R.
#'
#' @param id [\code{character(1)}]\cr
#'   Name of measure.
#'   Default is \dQuote{costs}.
#' @param alphas [\code{numeric}] \cr
#'   Numeric vector of alphas, which lies in [0, 1), representing the computed quantiles.
#'   Default: lower quantile alpha1 = 0.9, upper quantile alpha2 = 0.99 as we are interested in the performance of the scoring function in the the low density regions.
#' @param n.alpha [\code{numeric}] \cr
#'   Numeric discretization parameter greater than one, which splits the intervall of alpha1 and alpha2 as follows: {alpha1 + j * (alpha2-alpha1)/(n.alpha-1), j element of {0,...,n.alpha-1}}, Default: n.alpha = 50.
#' @param n.sim [\code{numeric(1)}] \cr
#'   Number of Monte-Carlo Samples, Default: 10^4.
#' @return [\code{numeric(1)}]
#'   Area under Mass-Volume Curve (AMV).
#' @references Thomas, A. et al. Learning Hyperparameters for Unsupervised Anomaly Detection,
#' ICML Anomaly Detection Workshop 2016
#' @inheritParams makeMeasure
#' @template ret_measure
#' @export
#' @family performance.
#'
makeAMVMeasure = function(id = "AMV", minimize = TRUE, alphas = c(0.9, 0.99), n.alpha = 50, n.sim = 10e4, best = 0, worst = NULL, name = id, note = "") {

  assertString(id)
  assertFlag(minimize)
  assertNumeric(alphas, lower = 0, upper = 1)
  assertCount(n.sim)
  assertString(name)
  assertString(note)

  makeMeasure(id = id, minimize = minimize, extra.args = list(alphas, n.sim),
    properties = c("oneclass", "req.task", "req.model", "req.pred", "predtype.prob", "req.feats"),
    best = best, worst = worst,
    fun = function(task, model, pred, feats, extra.args) {

      n.feat = getTaskNFeats(task)
      data = getTaskData(task, target.extra = TRUE)

      if (n.feat > 8) {
        warningf("Dimension might be too high for volume estimation. Apply feature resampling")
      }

      alpha.seq = c()
      for(j in seq_len(n.alpha-1)) {
        alpha.seq[j] = alphas[1] + j * (alphas[2]-alphas[1])/(n.alpha-1)
      }
        # vector of offsets for different alphas
        # type = 8: The resulting quantile estimates are approximately median-unbiased
        # regardless of the distribution of x.
        prob = getPredictionProbabilities(pred)[1]
        offsets = quantile(as.matrix(prob), 1 - alpha.seq, type = 8)

        ### Monte Carlo (MC) Integration for lambda

        # Compute hypercube where test data lies
        bounds = sapply(feats, FUN = function(x) c(min(x), max(x))) #falsche Daten, die müssen aus der prediction sein
        # Volume of the hypercube enclosing the data.
        volume = prod(bounds[2, ] - bounds[1,])

        # Sample nsim points from the hypercube (MCMC Samples)
        dfu = data.frame(Map(runif, n = n.sim, min = bounds[1, ], max = bounds[2, ]))
        colnames(dfu) = colnames(bounds)

        # get scores for sampled data from the hypercube
        su = predict(model, newdata = dfu)
        su = getPredictionProbabilities(su)[,1]

        # calculate volume via monte carlo (share of scores higher as the offset in relation to the whole volume of the hypercube)
        vol = sapply(offsets, function(offset) {mean(su >= offset)}) * volume

        ### MC end

        # Trapezoidal Integration for AMV
        sum.vol = vol[-length(vol)] * 2 + diff(vol)
        amv = ((diff(alpha.seq)) %*% (sum.vol) / 2)
      # Return area under mass-volume curve
      as.numeric(amv)
    },
    name = name,
    note = note
  )
}

