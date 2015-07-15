#' @title Binary Classification Measures for Multilabel Classification Predictions.
#'
#' @description
#' Measures the quality of each label prediction w.r.t. some binary classification 
#' performance measure.
#' 
#' @param pred [\code{\link{Prediction}}]\cr
#'   Multilabel Prediction object. 
#' @param measures [\code{\link{Measure}} | list of \code{\link{Measure}}]
#'   Performance measure(s) to evaluate. Has to be a measure that measures 
#'   binary classification performance. Default is mean missclassification error.
#' @return [named \code{matrix}]. Performance value(s), column names are measure(s), row names are labels.
#' @export
#' @family multilabel
#' @examples
#' task = yeast.task
#' lrn = makeMultilabelBinaryRelevanceWrapper("classif.rpart")
#' mod = train(lrn, yeast.task)
#' pred = predict(mod, yeast.task)
#' measures = list(mmce, acc)
#' getMultilabelBinaryPerformances(pred, measures)
#' # with prediction result of resample
#' rdesc = makeResampleDesc("CV", iters = 2)
#' r = resample(lrn, task, rdesc, measures = list(hamloss))
#' getMultilabelBinaryPerformances(r$pred, measures)
getMultilabelBinaryPerformances = function(pred, measures) {
  checkPrediction(pred, task.type = "multilabel")
  pred$task.desc$type = "classif"
  measures = checkMeasures(measures, pred$task.desc)
  p = matrix(, length(pred$task.desc$class.levels), length(measures))
  colnames(p) = vcapply(measures, measureAggrName)
  rownames(p) = pred$task.desc$class.levels
  truth = getPredictionTruth(pred)
  response = getPredictionResponse(pred)
  for (measure in measures) {
    predi = pred
    measurename = measureAggrName(measure)
    for (label in pred$task.desc$class.levels) {
      predi$data = data.frame(truth = truth[, label], response = response[, label])
      p[label, measurename] = performance(predi, measure)
    }
  }
  return(p)
}
