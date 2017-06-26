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
makeSubSamplingAMVMeasure = function(id = "SubSamplingAMV", minimize = TRUE, alphas = seq(from = 0.9, to = 0.99, by = 0.01), n.sim = 10e4, n.feat.subsampling = 3, n.draw.feat.subsampling = 50, best = 0, worst = NULL, name = id, note = "") {

  assertString(id)
  assertFlag(minimize)
  assertNumeric(alphas, lower = 0, upper = 1)
  assertCount(n.sim)
  # Curse of dimensionality for more than 5 features
  assertNumeric(n.feat.subsampling, lower = 0, upper = 5, null.ok = TRUE)
  assertNumeric(n.draw.feat.subsampling, lower = 0, null.ok = TRUE)
  assertString(name)
  assertString(note)

  measureAMV = makeAMVMeasure(id = "AMV", minimize = minimize, alphas = alphas, n.sim = n.sim, best = best, worst = worst, name = id)

  if(!is.null(n.feat.subsampling) && n.feat.subsampling >= 5) {
    warningf("Dimension might be too high for volume estimation. Choose a smaller feature subsampling size.")
  }

  makeMeasure(id = id, minimize = minimize, extra.args = list(n.feat.subsampling, n.draw.feat.subsampling, measureAMV),
    properties = c("oneclass", "req.task", "req.model", "req.pred", "predtype.prob"),
    best = best, worst = worst,
    fun = function(task, model, pred, feats, extra.args) {

      n.feat = getTaskNFeats(task)
      data = getTaskData(task)
      featurename = getTaskFeatureNames(task)
      targetname = getTaskTargetNames(task)

      n.feat.subsampling = extra.args[[1]]
      n.draw.feat.subsampling = extra.args[[2]]
      measureAMV = extra.args[[3]]

      if (n.feat.subsampling >= n.feat) {
        stopf("Cannot take a sample of (%i) of size (%i)", n.feat.subsampling, n.feat)
      }

        amv.k = c()
        for (k in 1:n.draw.feat.subsampling) {
          inds.feat.subsample = sample(n.feat, n.feat.subsampling) #index feature
          feat.subsample = featurename[inds.feat.subsample]
          sub.train.dat = data[, c(targetname, feat.subsample)]

          sub.test.data = #### Hier müssen die Daten aus Prediction rein.

          # train on train data
          sub.task = makeOneClassTask(data = sub.train.data, target = task$task.desc$target,
          positive = task$task.desc$positive, negative = task$task.desc$negative)

          sub.lrn = makeLearner(model$learner$id, predict.type = model$learner$predict.type,
            predict.threshold = model$learner$predict.threshold, par.vals = mod_svm_resp$learner$par.vals)
          sub.mod = train(sub.lrn, sub.task)

          # predict on new data from prediction
          sub.pred = predict(sub.mod, newdata = sub.test.data)

          amv.k[i] = performance(pred, measureAMV, task, model)
        }
        amv = mean(amv.k)
      # Return area under mass-volume curve
      as.numeric(amv)
    },
    name = name,
    note = note
  )
}
