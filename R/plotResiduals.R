#' @title Create box or violin plots for a BenchmarkResult.
#'
#' @description
#' Plots box or violin plots for a selected \code{measure} across all iterations
#' of the resampling strategy, faceted by the \code{task.id}.
#'
#' @template arg_bmr
#' @template arg_measure
#' @param style [\code{character(1)}]\cr
#'   Type of plot, can be \dQuote{box} for a boxplot or \dQuote{violin} for a violin plot.
#'   Default is \dQuote{box}.
#' @param pretty.names [\code{logical(1)}]\cr
#'   Whether to use the \code{\link{Measure}} name instead of the id in the plot.
#'   Default is \code{TRUE}.
#' @template arg_facet_nrow_ncol
#' @template arg_order_lrns
#' @template arg_order_tsks
#' @template ret_gg2
#' @family plot
#' @export
#' @examples
#' # see benchmark
plotResiduals = function(pred) {
# , measure = NULL, style = "box", order.lrns = NULL,
#  order.tsks = NULL, pretty.names = TRUE, facet.wrap.nrow = NULL, facet.wrap.ncol = NULL) {

  # assertClass(pred, c("PredictionClassif", "PredictionRegr"))
  assertClass(pred, "Prediction")

  df = as.data.frame(pred)

  p = ggplot(df, aes_string("truth", "response")) + geom_point()
  # p = p + theme(axis.title.x = element_blank(), axis.text.x = element_text(angle = -45, hjust = 0))

  
  # if (pretty.names)
  #  p = p + ylab(measure$name)

  return(p)
}
