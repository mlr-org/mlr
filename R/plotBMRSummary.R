#' @title Plot a benchmark summary.
#'
#' @description
#' Creates a scatter plot, where each line refers to a task.
#' On that line the aggregated scores for all learners are plotted, for that task.
#' Optionally, you can apply a rank transformation or just use one of ggplot2's transformations
#' like [ggplot2::scale_x_log10].
#'
#' @template arg_bmr
#' @template arg_measure
#' @param trafo (`character(1)`)\cr
#'   Currently either \dQuote{none} or \dQuote{rank}, the latter performing a rank transformation
#'   (with average handling of ties) of the scores per task.
#'   NB: You can add always add [ggplot2::scale_x_log10] to the result to put scores on a log scale.
#'   Default is \dQuote{none}.
#' @template arg_order_tsks
#' @param pointsize (`numeric(1)`)\cr
#'   Point size for ggplot2 [ggplot2::geom_point] for data points.
#'   Default is 4.
#' @param jitter (`numeric(1)`)\cr
#'   Small vertical jitter to deal with overplotting in case of equal scores.
#'   Default is 0.05.
#' @template arg_prettynames
#' @template ret_gg2
#' @family benchmark
#' @family plot
#' @export
#' @examples
#' # see benchmark
plotBMRSummary = function(bmr, measure = NULL, trafo = "none", order.tsks = NULL,
  pointsize = 4L, jitter = 0.05, pretty.names = TRUE) {

  assertClass(bmr, "BenchmarkResult")
  measure = checkBMRMeasure(measure, bmr)
  assertChoice(trafo, c("none", "rank"))
  meas.name = measureAggrName(measure)
  assertNumber(pointsize, lower = 0)
  assertNumber(jitter, lower = 0)

  df = getBMRAggrPerformances(bmr, as.df = TRUE)
  xlab.string = meas.name

  # trafo to ranks manually here
  if (trafo == "rank") {
    setDT(df)
    df[, get("meas.name") := rank(.SD[[meas.name]], ties.method = "average"), by = "task.id"] # nolint FIXME: find out what `:=` looks like in the AST and adjust the linter
    setDF(df)
    xlab.string = stri_paste("rank of", xlab.string, sep = " ")
  }

  df = orderBMRTasks(bmr, df, order.tsks)

  if (pretty.names) {
    learner.short.names = getBMRLearnerShortNames(bmr)
    checkDuplicatedLearnerNames(learner.short.names)
    levels(df$learner.id) = learner.short.names
  }

  # shape parameter is added and set to 19 later, so it can be changed
  p = ggplot(df, aes_string(x = meas.name, y = "task.id", col = "learner.id", shape = "learner.id"))
  p = p + geom_point(size = pointsize, position = position_jitter(width = 0, height = jitter))
  # set shape to standard shape 19
  p = p + scale_shape_manual(values = rep(19, length(getBMRLearnerIds(bmr))))
  # we dont need y label, the task names speak for themselves
  p = p + ylab("")
  p = p + xlab(xlab.string)

  return(p)
}
