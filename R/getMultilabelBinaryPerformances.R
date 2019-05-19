#' @title Retrieve binary classification measures for multilabel classification predictions.
#'
#' @description
#' Measures the quality of each binary label prediction w.r.t. some binary classification
#' performance measure.
#'
#' @param pred ([Prediction])\cr
#'   Multilabel Prediction object.
#' @param measures ([Measure] | list of [Measure])\cr
#'   Performance measure(s) to evaluate, must be applicable to binary classification performance.
#'   Default is `mmce`.
#' @return (named `matrix`). Performance value(s), column names are measure(s), row names are labels.
#' @export
#' @family multilabel
#' @examples
#' # see makeMultilabelBinaryRelevanceWrapper
getMultilabelBinaryPerformances = function(pred, measures) {

  checkPrediction(pred, task.type = "multilabel")
  measures = checkMeasures(measures, "classif")
  p = matrix(, length(pred$task.desc$class.levels), length(measures))
  colnames(p) = vcapply(measures, measureAggrName)
  rownames(p) = pred$task.desc$class.levels
  truths = getPredictionTruth(pred)
  responses = getPredictionResponse(pred)
  if (pred$predict.type == "prob") {
    probs = getPredictionProbabilities(pred)
  }
  for (measure in measures) {
    predi = pred
    predi$task.desc$type = "classif"
    predi$task.desc$class.levels = c("TRUE", "FALSE")
    predi$task.desc$positive = "TRUE"
    predi$task.desc$negative = "FALSE"
    measurename = measureAggrName(measure)
    for (label in pred$task.desc$class.levels) {
      predi$data = data.frame(truth = truths[, label], response = responses[, label])
      if (pred$predict.type == "prob") {
        predi$data$prob.TRUE = probs[, label]
      }
      p[label, measurename] = performance(predi, measure)
    }
  }
  return(p)
}
