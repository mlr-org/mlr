#' @title Generate threshold vs. performance(s) for multilabel classification.
#'
#' @description
#' Generates data on threshold vs. performance(s) for multilabel classification that can be used for plotting.
#' 
#' @family generate_plot_data
#' @family thresh_vs_perf
#' @family multilabel
#'
#' @template arg_plotroc_obj
#' @template arg_measures
#' @param gridsize [\code{integer(1)}]\cr
#'   Grid resolution for x-axis (threshold).
#'   Default is 100.
#' @param aggregate [\code{logical(1)}]\cr
#'   Whether to aggregate \code{\link{ResamplePrediction}}s or to plot the performance
#'   of each iteration separately.
#'   Default is \code{TRUE}.
#' @param task.id [\code{character(1)}]\cr
#'   Selected task in \code{\link{BenchmarkResult}} to do plots for, ignored otherwise.
#'   Default is first task.
#' @return [\code{MultilabelThreshVsPerfData}]. A named list containing the measured performance
#'   across the threshold grid, the measures, and whether the performance estimates were
#'   aggregated (only applicable for (list of) \code{\link{ResampleResult}}s).
#' @export
generateMultilabelThreshVsPerfData = function(obj, measures, gridsize = 100L, aggregate = TRUE, task.id = NULL)
  UseMethod("generateMultilabelThreshVsPerfData")

#' @export
generateMultilabelThreshVsPerfData.Prediction = function(obj, measures, gridsize = 100L, aggregate = TRUE,
                                                         task.id = NULL) {
  checkPrediction(obj, task.type = "multilabel", predict.type = "prob")
  generateMultilabelThreshVsPerfData.list(obj, measures, gridsize, aggregate, task.id)
}

#' @export
generateMultilabelThreshVsPerfData.ResampleResult = function(obj, measures, gridsize = 100L, aggregate = TRUE,
                                                             task.id = NULL) {
  obj = getRRPredictions(obj)
  checkPrediction(obj, task.type = "multilabel", predict.type = "prob")
  generateMultilabelThreshVsPerfData.Prediction(obj, measures, gridsize, aggregate)
}

#' @export
generateMultilabelThreshVsPerfData.BenchmarkResult = function(obj, measures, gridsize = 100L, aggregate = TRUE,
                                                              task.id = NULL) {
  tids = getBMRTaskIds(obj)
  if (is.null(task.id))
    task.id = tids[1L]
  else
    assertChoice(task.id, tids)
  obj = getBMRPredictions(obj, task.ids = task.id, as.df = FALSE)[[1L]]
  
  for (x in obj)
    checkPrediction(x, task.type = "multilabel", predict.type = "prob")
  lapply(obj, function(x) generateMultilabelThreshVsPerfData.list(obj[[1]], measures, gridsize, aggregate, task.id))
}

#' @export
generateMultilabelThreshVsPerfData.list = function(obj, measures, gridsize = 100L, aggregate = TRUE, task.id = NULL) {
  assertClass(obj, c("PredictionMultilabel"))
  measurenames = sapply(measures, function(x) x$id)
  probs = getPredictionProbabilities(obj)
  truths = getPredictionTruth(obj)
  responses = getPredictionResponse(obj)
  labels = obj$task.desc$class.levels
  predi = obj
  class(predi) = c("PredictionClassif", "Prediction")
  predi$task.desc$type = "classif"
  predi$task.desc$class.levels = c("TRUE", "FALSE")
  predi$task.desc$positive = "TRUE"
  predi$task.desc$negative = "FALSE"
  
  predi$data = data.frame(id = obj$data$id, truth = truths[, labels[1]], prob.TRUE = probs[, labels[1]], 
                          prob.FALSE = 1-probs[, labels[1]], response = responses[, labels[1]])
  p = generateThreshVsPerfData(predi, measures, gridsize = gridsize, aggregate = aggregate, task.id = NULL)
  
  p$data = data.frame(matrix(, gridsize, length(measures)*(length(obj$task.desc$class.levels))), p$data$threshold)
  colnames(p$data) = c(outer(measurenames, obj$task.desc$class.levels, FUN = paste, sep = "."), "threshold")
  
  for (label in obj$task.desc$class.levels) {
    predi$data = data.frame(id=obj$data$id, truth = truths[, label], prob.TRUE = probs[, label],
                            prob.FALSE = 1-probs[, label], response = responses[, label])
    p$data[, paste(measurenames, label, sep = ".")] = generateThreshVsPerfData(predi, measures, gridsize = gridsize, 
                                                                                         aggregate = aggregate, task.id = NULL)$data[, -3]
  }
  p$labels = labels
  class(p) = "MultilabelThreshVsPerfData"
  p
}

#' @title Plots ROC curves for Multilabel-classification using ggplot2
#'
#' @description
#' Plots ROC curves from predictions.
#'
#' @family plot
#' @family thresh_vs_perf
#' @family multilabel
#'
#' @template arg_plotroc_obj
#' @template arg_measures
#' @param diagonal [\code{logical(1)}]\cr
#'   Whether to plot a dashed diagonal line.
#'   Default is \code{TRUE}.
#' @param pretty.names [\code{logical(1)}]\cr
#'   Whether to use the \code{\link{Measure}} name instead of the id in the plot.
#'   Default is \code{TRUE}.
#' @template ret_ggv
#' @export
#' @examples
#' \donttest{
#' multilabel.lrn = makeLearner("classif.rpart", predict.type = "prob")
#' multilabel.lrn = makeMultilabelBinaryRelevanceWrapper(multilabel.lrn)
#' 
#' mod = train(multilabel.lrn, yeast.task)
#' pred = predict(mod, task = yeast.task, subset = 1:10)
#' roc = generateMultilabelThreshVsPerfData(pred, measures = list(fpr, tpr))
#' plotMultilabelROCCurves(roc)
#' }
plotMultilabelROCCurves = function(obj, measures = obj$measures[1:2], diagonal = TRUE, pretty.names = TRUE) {
  assertClass(obj, "MultilabelThreshVsPerfData")
  assertList(measures, "Measure", len = 2)
  assertFlag(diagonal)
  assertFlag(pretty.names)
  measurenames = sapply(measures, function(x) x$id)
  obji = obj
  ROCplots = vector(mode = "list", length = length(obj$labels))
  names(ROCplots) = obj$labels
  for (label in obj$labels) {
    obji$data = obj$data[, c(paste(measurenames, label, sep = "."), "threshold")]
    colnames(obji$data)[1:length(measurenames)] = measurenames
    class(obji) = "ThreshVsPerfData"
    ROCplots[[label]] = try(plotROCCurves(obji, measures = measures, diagonal = diagonal, pretty.names = pretty.names))
    try(print(ROCplots[[label]] + ggtitle(label)))
  }
  invisible(ROCplots)
}
