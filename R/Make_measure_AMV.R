#' @title Creates the measure Area under the Mass-Volume Curve (AMV) for Anomaly detection (oneclass) for data dimension less than 8
#'
#' @description
#' Creates a measure for oneclass classification. The measure computes the
#' Area under the Mass-Volume Curve via Monte-Carlo approximation
#' of the diagonal. It uses the trapezoidal rule as implemented in
#' package \code{caTools} for integration. As AMV is based in a Monte-Carlo approximation
#' the curse of dimensionality applies for high dimensional data
#' (here: dimension greater than eight, see \code{makeAMVhdMeasure}).
#' The implementation is based on the python implementation:
#' https://github.com/albertcthomas/anomaly_tuning.
#' The difference is the type of quantile used, as the python default is not
#' available in R.
#' Note: prediction object must have \code{pred.type = 'prob'}
#'
#' @param id [\code{character(1)}]\cr
#'   Name of measure. Note: need to keep the string "AMV" in the ID if doing nested resampling.
#'   Default is \dQuote{AMV}.
#' @param alphas [\code{numeric}] \cr
#'   Numeric vector of alphas, which lies in [0, 1), representing the computed quantiles.
#'   Default: lower quantile alpha1 = 0.9, upper quantile alpha2 = 0.99 as we are interested in the performance of the scoring function in the the low density regions.
#' @param n.alpha [\code{numeric}] \cr
#'   Numeric discretization parameter greater than one, which splits the intervall of alpha1 and alpha2 as follows: {alpha1 + j * (alpha2-alpha1)/(n.alpha-1), j element of {0,...,n.alpha-1}}, Default: n.alpha = 50.
#' @param n.sim [\code{numeric(1)}] \cr
#'   Number of Monte-Carlo Samples, Default is 10^3.
#' @return [\code{numeric(1)}]
#'   Area under the Mass-Volume Curve (AMV).
#' @references Thomas, A. et al. Learning Hyperparameters for Unsupervised Anomaly Detection,
#' ICML Anomaly Detection Workshop 2016
#' @inheritParams makeMeasure
#' @template ret_measure
#' @export
#' @family performance.
#' @examples
#' # create an AMV measure which calculates the area under the Mass-Volume curve
#' #between 0.8 and 0.99 with 50 steps.
#' AMV = makeAMVMeasure(id = "AMV", minimize = TRUE, alphas = c(0.8, 0.99),
#' n.alpha = 50, n.sim = 10e3, best = 0, worst = NULL)
#'
#' data = getTaskData(oneclass2d.task)
#' inds.split = BBmisc::chunk(seq_len(nrow(data)), shuffle = TRUE, props = c(0.6, 0.4))
#' train.inds = inds.split[[1]]
#' test.inds = inds.split[[2]]
#' lrn = makeLearner("oneclass.svm", predict.type = "prob")
#' mod = train(lrn, oneclass2d.task, subset = train.inds)
#' pred = predict(mod, oneclass2d.task, subset = test.inds)
#'
#' # calculate performance for prediction object, pass data of features used for
#' # prediction as feats in performance
#' performance(pred = pred, measures = list(AMV), model = mod, task = oneclass2d.task)


makeAMVMeasure = function(id = "AMV", minimize = TRUE, alphas = c(0.9, 0.99), n.alpha = 50, n.sim = 1e+03, best = 0, worst = NULL, name = id, note = "") {

  assertString(id)
  assertFlag(minimize)
  assertNumeric(alphas, lower = 0, upper = 1)
  assertCount(n.sim)
  assertString(name)
  assertString(note)

  makeMeasure(id = id, minimize = minimize, extra.args = list(alphas = alphas, n.sim = n.sim),
    properties = c("oneclass", "req.model", "req.pred", "predtype.prob", "req.feats"),
    best = best, worst = worst,
    fun = function(task, model, pred, feats, extra.args) {
      alphas = extra.args[[1]]
      n.sim = extra.args[[2]]

      if (is.null(feats)) {
        if (!is.null(task)){
          subset.inds = model$subset
          data = getTaskData(task, target.extra = TRUE)$data
          if (length(subset.inds) != nrow(data)) {
            feats = data[subset.inds, ]
          }
        }
      }

      if (ncol(feats) > 8) {
        warningf("Dimension might be too high for volume estimation with AMV. Use AMVhd.")
      }
      alpha.seq = vector()
      for (j in seq_len(n.alpha - 1)) {
        alpha.seq[j] = alphas[1] + j * (alphas[2] - alphas[1]) / (n.alpha - 1)
      }
      # vector of offsets for different alphas
      # type = 8: The resulting quantile estimates are approximately median-unbiased
      # regardless of the distribution of x.

      # the reference paper states that it uses scores/prob that indicates anomaly
      # if the score is low when calculating the AUMVC(hd)
      # However, within mlR high prediction probability has a reversed interpretation,
      # therefore use prob of the normal class
      # (as here high prob are indication for normal observation)
      # to stay consistent with the theory in the reference paper.
      prob = getPredictionProbabilities(pred, cl = model$task.desc$negative)
      #set na.rm = TRUE
      offsets = quantile(as.matrix(prob), 1 - alpha.seq, type = 8)

      ### Monte Carlo (MC) Integration for lambda

      # Compute hypercube where data lies
      bounds = sapply(feats, FUN = function(x) c(min(x), max(x)))

      # Volume of the hypercube enclosing the test data.
      volume = prod(bounds[2, ] - bounds[1, ])

      # Sample nsim points from the hypercube (MCMC Samples)
      dfu = data.frame(Map(runif, n = n.sim, min = bounds[1, ], max = bounds[2, ]))
      colnames(dfu) = colnames(bounds)

      # get scores for sampled test data from the hypercube
      su = predict(model, newdata = dfu)
      su = getPredictionProbabilities(su, cl = model$task.desc$negative)

      # calculate volume via monte carlo
      # (share of scores higher as the offset in relation to the whole volume of the hypercube)
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

