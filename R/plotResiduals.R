#' @title Create residual plots for prediction objects or benchmark results.
#'
#' @description
#' Plots for model diagnostics. Provides scatterplots of true vs. predicted values
#' and histograms of the model's residuals.
#'
#' @param obj ([Prediction] | [BenchmarkResult])\cr
#'   Input data.
#' @param type Type of plot. Can be \dQuote{scatterplot}, the default. Or
#'   \dQuote{hist}, for a histogram, or in case of classification problems
#'   a barplot, displaying the residuals.
#' @param loess.smooth (`logical(1)`)\cr
#'   Should a loess smoother be added to the plot? Defaults to `TRUE`.
#'   Only applicable for regression tasks and if `type` is set to `scatterplot`.
#' @param rug (`logical(1)`)\cr
#'   Should marginal distributions be added to the plot? Defaults to `TRUE`.
#'   Only applicable for regression tasks and if `type` is set to `scatterplot`.
#' @param pretty.names (`logical(1)`)\cr
#'   Whether to use the short name of the learner instead of its ID in labels.
#'   Defaults to `TRUE`. \cr
#'   Only applicable if a [BenchmarkResult]
#'   is passed to `obj` in the function call, ignored otherwise.
#' @template ret_gg2
#' @family plot
#' @export
plotResiduals = function(obj, type = "scatterplot", loess.smooth = TRUE,
  rug = TRUE, pretty.names = TRUE) {
  assertChoice(type, c("scatterplot", "hist"))
  assertLogical(loess.smooth, len = 1L)
  assertLogical(rug, len = 1L)
  assertLogical(pretty.names, len = 1L)
  UseMethod("plotResiduals")
}

#' @export
plotResiduals.Prediction = function(obj, type = "scatterplot", loess.smooth = TRUE,
  rug = TRUE, pretty.names = TRUE) {

  task.type = obj$task.desc$type
  if (task.type %nin% c("regr", "classif")) {
    stopf("Task type must be 'regr' or 'classif'. But has type '%s'.", task.type)
  }

  df = as.data.frame(obj)

  p = makeResidualPlot(df, type, loess.smooth, rug, task.type)

  return(p)
}

#' @export
plotResiduals.BenchmarkResult = function(obj, type = "scatterplot", loess.smooth = TRUE,
  rug = TRUE, pretty.names = TRUE) {

  task.type = getBMRObjects(obj, as.df = TRUE, fun = function(X) {
    getRRTaskDesc(X)$type
  })
  task.type = unique(task.type$p)

  if (task.type %nin% c("regr", "classif")) {
    stopf("Task type must be 'regr' or 'classif'. But has type '%s'.", task.type)
  }

  df = getBMRPredictions(obj, as.df = TRUE)

  if (pretty.names) {
    learner.short.names = getBMRLearnerShortNames(obj)
    checkDuplicatedLearnerNames(learner.short.names)
    levels(df$learner.id) = learner.short.names
  }

  p = makeResidualPlot(df, type, loess.smooth, rug, task.type)

  p = p + facet_wrap(learner.id ~ task.id, scales = "free")

  return(p)
}

makeResidualPlot = function(df, type = "scatterplot", loess.smooth = TRUE,
  rug = TRUE, task.type) {
  if (type == "scatterplot") {
    p = ggplot(df, aes_string("truth", "response"))
    if (task.type == "classif") {
      p = p + geom_count()
    } else {
      p = p + geom_point()

      if (loess.smooth) {
        p = p + geom_smooth(se = FALSE)
      }
      if (rug) {
        p = p + geom_rug(color = "red")
      }
    }
    p = p + ggtitle("True value vs. fitted value")
  } else {
    df$residuals = as.numeric(df$truth) - as.numeric(df$response)
    p = ggplot(df, aes_string("residuals"))
    if (task.type == "classif") {
      p = p + geom_bar()
    } else {
      p = p + geom_histogram()
    }
    p = p + ggtitle("Histogram of residuals")
  }

  return(p)
}
