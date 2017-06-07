#' @title Compute Area under Mass-Volume Curve (AMV)
#' @description
#' Computes the Area under the Mass-Volume Curve via Monte-Carlo approximation
#' of the diagonal. Uses the trapezoidal rule as implemented in
#' package \code{caTools} for integration. The implementation is based on the
#' python implementation: \link{https://github.com/albertcthomas/anomaly_tuning}.
#' Differences are the type of quantile used, as the python default is not
#' available in R.
#'
#' @param model [\code{list}]  \cr
#'   Model used for scoring data and mcmc samples.
#' @param data [\code{data.frame | matrix}]  \cr
#'   Data to compute the AMV for.
#' @param predictions [\code{numeric}] \cr
#'   Model scores for data from model. If \code{NULL}, the data is scored using
#'   predict(model, newdata = data, predicttype = "distance").
#' @param alphas [\code{numeric}] \cr
#'   Numeric vector of alphas from 0 to 1, representing the computed quantiles.
#'   Default: 0.05, 0.1, 0.15, ... , 1.
#' @param scorefun [\code{function}] \cr
#'   Scoring function, such as the prediction function from a trained model.
#' @param nsim [\code{numeric(1)}] \cr
#'   Number of Monte-Carlo Samples, Default: 10^4.
#' @return [\code{numeric(1)}]
#'   Area under Mass-Volume Curve (AMV).
#' @references Thomas, A. et al. Learning Hyperparameters for Unsupervised Anomaly Detection,
#' ICML Anomaly Detection Workshop 2016
#' @export
calculateAMVMeasures = function(task, model, pred, alphas = seq(from = 0.05, to = 1, by = 0.05), nsim = 10e3) {

  assertNumeric(alphas, lower = 0, upper = 1)
  assertCount(nsim)

  n.feat = sum(task$task.desc$n.feat)
  data = task$env$data[,1:n.feat]

  if (model$learner$predict.type != "prob") {
    stopf("For calculation of the Mass Volume Curve of a scoring function predict.type =\"prob\" is required.")
  }
  if (n.feat >=5) {
    warningf("Dimension might be too high for volume estimation")
  }

  # vector of offsets for different alphas
  # type = 8: The resulting quantile estimates are approximately median-unbiased
  # regardless of the distribution of x.
    prob = getPredictionProbabilities(pred)[1]
    offsets = quantile(as.matrix(prob), 1 - alphas, type = 8)

  ### Monte Carlo (MC) Integration for lambda

  # Compute hypercube where data lies
  bounds = sapply(data, FUN = function(x) c(min(x), max(x)))
  # Volume of the hypercube enclosing the data.
  volume = prod(bounds[2, ] - bounds[1,])

  # Sample nsim points from the hypercube (MCMC Samples)
  dfu = data.frame(Map(runif, n = nsim, min = bounds[1, ], max = bounds[2, ]))
  colnames(dfu) = colnames(bounds)

  # get scores for sampled data from the hypercube
  su = predict(model, newdata = dfu)
  su = getPredictionProbabilities(su)[,1]

  # calculate volume via monte carlo (share of scores higher as the offset in relation to the whole volume of the hypercube)
  vol = sapply(offsets, function(offset) {mean(su >= offset)}) * volume

  ### MC end

  # Trapezoidal Integration for AMV
    sum.vol = vol[-length(vol)] * 2 + diff(vol)
    amv = ((diff(alphas)) %*% (sum.vol) / 2) / volume

    # Return area under mass-volume curve
    return(as.numeric(amv))
}

