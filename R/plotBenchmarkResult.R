#' @title Create a Trellis-plot for a selected measure.
#'
#' @description
#' Plots boxplots for a selected \code{measure} accross all iterations
#' of the resampling strategy, faceted by the \code{task.id}
#'
#' @template arg_bmr
#' @template arg_measure
#' @param style [\code{character(1)}]\cr
#'   Type of plot, can be \dQuote{box} for a boxplot or \dQuote{violin} for
#'   a violin-plot.
#'   Default is \dQuote{box}.
#' @param pretty.names [\code{logical(1)}]\cr
#'  Whether to use the \code{\link{Measure}} name instead of the id in the plot.
#'  Default is \code{TRUE}.
#' @template arg_order_lrns
#' @template arg_order_tsks
#' @template ret_gg2
#'
#' @references Manuel J. A. Eugster, Torsten Hothorn and Friedrich Leisch;
#' Domain-Based Benchmark Experiments: Exploratory and Inferential Analysis,
#' AUSTRIAN JOURNAL OF STATISTICS Volume 41 (2012), Number 1, 5–26,
#' but does not include any clustering or sorting.
#'
#' @examples
#' lrns = list(makeLearner("classif.nnet"), makeLearner("classif.rpart"))
#' tasks = list(iris.task, sonar.task)
#' rdesc = makeResampleDesc("CV", iters = 2L)
#' meas = list(acc, mmce, ber, featperc)
#' res = benchmark(lrns, tasks, rdesc, meas)
#' plotBenchmarkResult(res, acc)
#'
#' @family plot
#' @family benchmark
#' @export
plotBenchmarkResult = function(bmr, measure = NULL, style = "box", order.lrns = NULL,
                               order.tsks = NULL, pretty.names = TRUE) {
  choices = c("learner.id", "task.id")
  assertClass(bmr, "BenchmarkResult")
  assertClass(style, "character")
  assertChoice(style, c("box", "violin"))
   if (is.null(measure))
    measure = getBMRMeasures(bmr)[[1L]]
  assertClass(measure, "Measure")
  assertChoice(measure$id, getBMRMeasureIds(bmr))

  df = as.data.frame(bmr)
  if (!is.null(order.lrns))
    df = orderBMRLrns(bmr, df, order.lrns)
  if (!is.null(order.tsks))
    df = orderBMRTasks(bmr, df, order.tsks)

  if (pretty.names) {
    colnames(df) = mapValues(colnames(df), measure$id, measure$name)
    # add backticks to allow spaces and other weird characters
    mname = c("`", measure$name, "`", sep = "")
  } else
    mname = measure$id

  p = ggplot(df, aes_string("learner.id", mname))
  p = p + theme(axis.title.x = element_blank(),
                axis.text.x = element_text(angle = -45, hjust = 0))

  if (style == "box")
    p = p + geom_boxplot()
  else
    p = p + geom_violin() + stat_summary(fun.ymin = median, fun.ymax = median,
                                         fun.y = median, geom = "crossbar")
  return(p)
}
