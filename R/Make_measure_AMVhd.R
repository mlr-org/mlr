#' @title Creates the measure Area under Mass-Volume Curve (AMV) for Anomaly detection (oneclass) for high dimensional data
#'
#' @description
#' Creates a measure for oneclass classification on high dimensional data
#' (recommend for dimension greater than 8) called AMVhd, which is based on the
#' Area under the Mass-Volume Curve (AMV) (see \code{makeAMVMeasure}).
#' The basic idea is to do several feature sub-samplings (of dimension less than 8)
#' to reduce the dimension of the data, therefore AMV can be applied on each
#' subsamples, yielding partial scores AMV_k. The mean of the partial scores is
#' the new performancecriteria AMVhd.
#'
#' @return [\code{numeric(1)}]
#'   Area under Mass-Volume Curve (AMV) for high dimensional data.
#' @references Nicolas, G. How to Evaluate the Quality of Unsupervised Anomaly Detection Algorithms,
#' arXiv preprint arXiv:1607.01152
#' @inheritParams makeMeasure
#' @inheritParams makeAMVMeasure
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
