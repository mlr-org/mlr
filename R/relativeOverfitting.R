#' Estimate relative overfitting.
#'
#' Estimates the relative overfitting of a model as the ratio of the difference in test and train performance to the difference of test performance in the no-information case and train performance.
#' In the no-information case the features carry no information with respect to the prediction. This is simulated by permuting features and predictions.
#'
#' Currently only support for classification and regression tasks is implemented.
#'
#' @param predish ([ResampleDesc] | [ResamplePrediction] | [Prediction])\cr
#'   Resampling strategy or resampling prediction or test predictions.
#' @template arg_measures
#' @template arg_task
#' @template arg_learner
#' @param pred.train ([Prediction])\cr
#'   Training predictions. Only needed if test predictions are passed.
#' @param iter ([integer])\cr
#'   Iteration number. Default 1, usually you don't need to specify this. Only needed if test predictions are passed.
#' @return ([data.frame]). Relative overfitting estimate(s), named by measure(s), for each resampling iteration.
#' @export
#' @family performance
#' @references Bradley Efron and Robert Tibshirani; Improvements on Cross-Validation: The .632+ Bootstrap Method, Journal of the American Statistical Association, Vol. 92, No. 438. (Jun., 1997), pp. 548-560.
#' @examples
#' task = makeClassifTask(data = iris, target = "Species")
#' rdesc = makeResampleDesc("CV", iters = 2)
#' estimateRelativeOverfitting(rdesc, acc, task, makeLearner("classif.knn"))
#' estimateRelativeOverfitting(rdesc, acc, task, makeLearner("classif.lda"))
#' rpred = resample("classif.knn", task, rdesc)$pred
#' estimateRelativeOverfitting(rpred, acc, task)
#' @name estimateRelativeOverfitting
#' @rdname estimateRelativeOverfitting
estimateRelativeOverfitting = function(predish, measures, task, learner = NULL, pred.train = NULL, iter = 1) {
  assertClass(task, classes = "Task")
  UseMethod("estimateRelativeOverfitting")
}

#' @export
estimateRelativeOverfitting.ResampleDesc = function(predish, measures, task, learner, ...) {
  assertClass(learner, classes = "Learner")
  measures = checkMeasures(measures, task)

  predish$predict = "both"
  r = resample(learner, task, predish, measures, show.info = FALSE)

  estimateRelativeOverfitting(r$pred, measures, task)
}

#' @export
estimateRelativeOverfitting.ResamplePrediction = function(predish, measures, task, ...) {
  measures = checkMeasures(measures, task)
  mids = vcapply(measures, function(m) m$id)

  iterations = unique(predish$data$iter)
  rbindlist(lapply(iterations, function(i) {
    data = predish$data[predish$data$iter == i & predish$data$set == "test", ]
    pred.test = makePrediction(task$task.desc, row.names(data), data$id, data$truth, predish$predict.type, predish$predict.threshold, data$response, predish$time[i])

    data = predish$data[predish$data$iter != i & predish$data$set == "test", ]
    pred.train = makePrediction(task$task.desc, row.names(data), data$id, data$truth, predish$predict.type, predish$predict.threshold, data$response, predish$time[i])

    estimateRelativeOverfitting(pred.test, measures, task, pred.train = pred.train, iter = i)
  }), use.names = TRUE)
}

#' @export
estimateRelativeOverfitting.Prediction = function(predish, measures, task, learner, pred.train, iter = 1) {

  assertClass(pred.train, classes = "Prediction")
  measures = checkMeasures(measures, task)
  mids = vcapply(measures, function(m) m$id)

  perf.test = performance(predish, measures = measures)
  perf.train = performance(pred.train, measures = measures)

  nrows = nrow(predish$data) + nrow(pred.train$data)
  pred.permuted = predish
  # generate all permutations of the predictions
  pred.permuted$data = data.frame(truth = rep(c(predish$data$truth, pred.train$data$truth), each = nrows),
    response = rep(c(predish$data$response, pred.train$data$response), times = nrows))
  perf.permuted = performance(pred.permuted, measures = measures, task = task)

  df = (perf.test - perf.train) / (perf.permuted - perf.train)
  names(df) = paste("relative.overfit", mids, sep = ".")
  cbind(data.frame(iter = iter), t(data.frame(df)))
}
