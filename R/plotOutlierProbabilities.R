#' @title Create outlier score plots for prediction objects or benchmark results.
#'
#' @description
#' Provides scatterplots of sorted outlier scores.
#'
#' @param obj [\code{\link{Prediction}}]\cr
#'   Input data.
#' @family plot
#' @export
plotOutlierProbabilities = function(pred) {
  task.type = pred$task.desc$type
  if (task.type %nin% c("oneclass"))
    stopf("Task type must be 'oneclass'. But has type '%s'.", task.type)

  # only need one prob column
  prob = getPredictionProbabilities(pred)[,1]
  yhat = getPredictionResponse(pred)
  y = getPredictionTruth(pred)

  if (!is.null(y)) {
    df = data.frame(truth = y, probability = prob, predictedResponse = yhat)
    df$.err = y != yhat #false predicted
  } else {
    df = data.frame(probability = prob, predictedResponse = yhat)
  }

  # to plot the ordered probabilities
  df = df[order(prob),]
  df = data.frame(index = 1:length(prob), df)
  th = pred$threshold[1]

  p = ggplot(df, aes(x = index, y = probability, color = predictedResponse)) +
    geom_point() +
    xlab("sorted observation") + ylab("outlier probability") +
    labs(color = "predicted response") +
    geom_hline(aes(yintercept = th, linetype = "setted threshold")) +
    scale_linetype_manual(name = "", values = 1, guide = guide_legend()) +
    aes(shape = factor(truth, levels = c("TRUE", "FALSE"), labels = c("21", "24")), fill = predictedResponse) + guides(fill=FALSE) +
    scale_shape_manual(name = "truth", labels = c("TRUE", "FALSE"), values = c(21, 24))

  # plot white borders for false predicted values
  sub = subset(df, df$.err)
  p = p + geom_point(data = sub, aes(x = index, y = probability), size = 2,stroke = 1, color = "white") +
    theme(legend.key = element_rect(fill = "darkgrey"))

  return(p)
}
