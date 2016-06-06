#' Estimate relative overfitting.
#'
#' Estimates the relative overfitting of a model as the ratio of the difference in test and train performance to the difference of test performance in the no-information case and train performance.
#' In the no-information case the features carry no information with respect to the prediction. This is simulated by permuting features and predictions.
#'
#' Currently only support for classification and regression tasks is implemented.
#'
#' @param rdesc [\code{\link{ResampleDesc}}]\cr
#'   Resampling strategy.
#' @template arg_measures
#' @template arg_task
#' @template arg_learner
#' @return [\code{data.frame}]. Relative overfitting estimate(s), named by measure(s), for each resampling iteration.
#' @export
#' @family performance
#' @references Bradley Efron and Robert Tibshirani; Improvements on Cross-Validation: The .632+ Bootstrap Method, Journal of the American Statistical Association, Vol. 92, No. 438. (Jun., 1997), pp. 548-560.
#' @examples
#' task = makeClassifTask(data = iris, target = "Species")
#' rdesc = makeResampleDesc("CV", iters = 2)
#' estimateRelativeOverfitting(rdesc, acc, task, makeLearner("classif.knn"))
#' estimateRelativeOverfitting(rdesc, acc, task, makeLearner("classif.lda"))
estimateRelativeOverfitting = function(rdesc, measures, task, learner) {
  assertClass(rdesc, classes = "ResampleDesc")
  assertClass(task, classes = "Task")
  assertClass(learner, classes = "Learner")

  UseMethod("estimateRelativeOverfitting")
}

#' @export
#' @rdname estimateRelativeOverfitting
estimateRelativeOverfitting.ResampleDesc = function(rdesc, measures, task, learner) {
  measures = checkMeasures(measures, task)

  rdesc$predict = "both"
  r = resample(learner, task, rdesc, measures, show.info = FALSE)
  mids = vcapply(measures, function(m) m$id)

  iterations = nrow(r$measures.test)
  do.call(rbind, lapply(1:iterations, function(i) {
    perf.test = r$measures.test[i,mids,drop = FALSE]
    perf.train = r$measures.train[i,mids,drop = FALSE]

    data = r$pred$data[r$pred$data$iter == i & r$pred$data$set == "test",]
    nrows = nrow(data)
    pred.permuted = r$pred
    pred.permuted$data = data.frame(truth = rep(data$truth, each = nrows),
      response = rep(data$response, times = nrows))
    perf.permuted = performance(pred.permuted, measures = measures, task = task)

    df = (perf.test - perf.train) / (perf.permuted - perf.train)
    names(df) = paste("relative.overfit", mids, sep = ".")
    cbind(data.frame(iter = i), df)
  }))
}
