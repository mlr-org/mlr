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
#' @export
#' @rdname estimateRelativeOverfitting
estimateRelativeOverfitting.ResampleDesc = function(rdesc, measures, task, learner) {
  assertClass(rdesc, classes = "ResampleDesc")
  assertClass(learner, classes = "Learner")
  measures = checkMeasures(measures, task)

  rdesc$predict = "both"
  r = resample(learner, task, rdesc, measures, show.info = FALSE)

  estimateRelativeOverfitting(r$pred, measures, task)
}

#' Estimate relative overfitting.
#'
#' Estimates the relative overfitting of a model as the ratio of the difference in test and train performance to the difference of test performance in the no-information case and train performance.
#' In the no-information case the features carry no information with respect to the prediction. This is simulated by permuting features and predictions.
#'
#' Currently only support for classification and regression tasks is implemented.
#'
#' @param rpred [\code{\link{ResamplePrediction}}]\cr
#'   Resampling prediction.
#' @template arg_measures
#' @template arg_task
#' @return [\code{data.frame}]. Relative overfitting estimate(s), named by measure(s), for each resampling iteration.
#' @export
#' @family performance
#' @references Bradley Efron and Robert Tibshirani; Improvements on Cross-Validation: The .632+ Bootstrap Method, Journal of the American Statistical Association, Vol. 92, No. 438. (Jun., 1997), pp. 548-560.
#' @examples
#' task = makeClassifTask(data = iris, target = "Species")
#' rdesc = makeResampleDesc("CV", iters = 2)
#' rpred = resample("classif.knn", task, rdesc)$pred
#' estimateRelativeOverfitting(rpred, acc, task)
#' @export
#' @rdname estimateRelativeOverfitting
estimateRelativeOverfitting.ResamplePrediction = function(rpred, measures, task) {
  assertClass(rpred, classes = "ResamplePrediction")
  measures = checkMeasures(measures, task)
  mids = vcapply(measures, function(m) m$id)

  iterations = unique(rpred$data$iter)
  rbind.fill(lapply(iterations, function(i) {
    data = rpred$data[rpred$data$iter == i & rpred$data$set == "test",]
    pred.test = makePrediction(task$task.desc, row.names(data), data$id, data$truth, rpred$predict.type, rpred$predict.threshold, data$response, rpred$time[i])

    data = rpred$data[rpred$data$iter != i & rpred$data$set == "test",]
    pred.train = makePrediction(task$task.desc, row.names(data), data$id, data$truth, rpred$predict.type, rpred$predict.threshold, data$response, rpred$time[i])

    estimateRelativeOverfitting(pred.test, measures, task, pred.train, i)
  }))
}

#' Estimate relative overfitting.
#'
#' Estimates the relative overfitting of a model as the ratio of the difference in test and train performance to the difference of test performance in the no-information case and train performance.
#' In the no-information case the features carry no information with respect to the prediction. This is simulated by permuting features and predictions.
#'
#' Currently only support for classification and regression tasks is implemented.
#'
#' @param pred.test [\code{\link{Prediction}}]\cr
#'   Test predictions.
#' @template arg_measures
#' @template arg_task
#' @param pred.train [\code{\link{Prediction}}]\cr
#'   Training predictions.
#' @param iter [\code{\link{integer}}]\cr
#'   Iteration number. Default 1, usually you don't need to specify this.
#' @return [\code{data.frame}]. Relative overfitting estimate(s), named by measure(s), for each resampling iteration.
#' @export
#' @family performance
#' @references Bradley Efron and Robert Tibshirani; Improvements on Cross-Validation: The .632+ Bootstrap Method, Journal of the American Statistical Association, Vol. 92, No. 438. (Jun., 1997), pp. 548-560.
#' @examples
#' task = makeClassifTask(data = iris, target = "Species")
#' mod = train(makeLearner("classif.knn"), task, subset = 1:100)
#' pred.train = predict(mod, task, subset = 1:100)
#' pred.test = predict(mod, task, subset = 101:150)
#' estimateRelativeOverfitting(pred.test, acc, task, pred.train)
#' @export
#' @rdname estimateRelativeOverfitting
estimateRelativeOverfitting.Prediction = function(pred.test, measures, task, pred.train, iter = 1) {
  assertClass(pred.test, classes = "Prediction")
  assertClass(pred.train, classes = "Prediction")
  measures = checkMeasures(measures, task)
  mids = vcapply(measures, function(m) m$id)

  perf.test = performance(pred.test, measures = measures)
  perf.train = performance(pred.train, measures = measures)

  nrows = nrow(pred.test$data) + nrow(pred.train$data)
  pred.permuted = pred.test
  pred.permuted$data = data.frame(truth = rep(c(pred.test$data$truth, pred.train$data$truth), each = nrows),
    response = rep(c(pred.test$data$response, pred.train$data$response), times = nrows))
  perf.permuted = performance(pred.permuted, measures = measures, task = task)

  df = (perf.test - perf.train) / (perf.permuted - perf.train)
  names(df) = paste("relative.overfit", mids, sep = ".")
  cbind(data.frame(iter = iter), t(data.frame(df)))
}

estimateRelativeOverfitting = function(predish, measures, task, ...) {
  assertClass(task, classes = "Task")
  UseMethod("estimateRelativeOverfitting")
}
