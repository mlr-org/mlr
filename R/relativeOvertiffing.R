#' Estimate relative overfitting.
#'
#' Estimates the relative overfitting of a model as the ratio of the difference in test and train performane to the difference of test performance in the no-information case and train performance.
#' In the no-information case the features carry no information with respect to the prediction. This is simulated by permuting features and predictions.
#'
#' Currently only support for classification and regression tasks is implemented.
#'
#' @template arg_pred
#' @template arg_measures
#' @param task [\code{\link{Task}}]\cr
#'   Learning task.
#' @param model [\code{\link{WrappedModel}}]\cr
#'   Model built on training data.
#' @param feats [\code{data.frame}]\cr
#'   Features of predicted data, usually not needed except for clustering.
#'   If the prediction was generated from a \code{task}, you can also pass this instead and the features
#'   are extracted from it.
#' @return [named \code{numeric}]. Relative overfitting estimate(s), named by measure(s).
#' @export
#' @family performance
#' @references Bradley Efron and Robert Tibshirani; Improvements on Cross-Validation: The .632+ Bootstrap Method, Journal of the American Statistical Association, Vol. 92, No. 438. (Jun., 1997), pp. 548-560.
#' @examples
#' training.set = seq(1, nrow(iris), by = 2)
#' test.set = seq(2, nrow(iris), by = 2)
#'
#' task = makeClassifTask(data = iris, target = "Species")
#' lrn = makeLearner("classif.lda")
#' mod = train(lrn, task, subset = training.set)
#' pred = predict(mod, newdata = iris[test.set, ])
#' relativeOverfitting(pred, measures = mmce, task = task, model = mod)
#'
#' lrn2 = makeLearner("classif.knn")
#' mod2 = train(lrn2, task, subset = training.set)
#' pred2 = predict(mod2, newdata = iris[test.set, ])
#' relativeOverfitting(pred2, measures = mmce, task = task, model = mod2)
relativeOverfitting = function(pred, measures, task, model, feats = NULL) {
  assertClass(pred, classes = "Prediction")
  if (is.null(pred$data$truth))
    stopf("You need to have a 'truth' column in your pred object to estimate the relative overfitting!")
  assertClass(model, classes = "WrappedModel")
  assertClass(task, classes = "Task")

  type = getTaskDescription(pred)$type
  assertChoice(type, choices = c("classif", "regr"))

  perf.test = performance(pred, measures = measures, task = task, model = model, feats = feats)

  nrows = nrow(pred$data)
  pred.permuted = pred
  pred.permuted$data = data.frame(truth = rep(pred$data$truth, each = nrows),
    response = rep(pred$data$response, times = nrows))
  perf.permuted = performance(pred.permuted, measures = measures, task = task, model = model, feats = feats)

  pred.train = predict(model, task)
  perf.train = performance(pred.train, measures = measures, task = task, model = model, feats = feats)

  (perf.test - perf.train) / (perf.permuted - perf.train)
}
