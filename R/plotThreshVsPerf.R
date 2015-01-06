#' @title Plot threshold vs. performance(s) for 2-class classification.
#'
#' @template arg_pred
#' @template arg_measures
#' @param mark.th [\code{numeric(1)}]\cr
#'   Mark given threshold with vertical line?
#'   Default is \code{NA} which means not to do it.
#' @param gridsize [\code{integer(1)}]\cr
#'   Grid resolution for x-axis (threshold).
#'   Default is 100.
#' @param linesize [\code{numeric(1)}]\cr
#'   Linesize for ggplot2 \code{\link[ggplot2]{geom_line}} for performance graphs.
#'   Default is 1.5.
#' @template ret_gg2
#' @export
plotThreshVsPerf = function(pred, measures, mark.th = NA_real_, gridsize = 100L, linesize = 1.5) {
  assertClass(pred, classes = "Prediction")
  td = pred$task.desc
  if (td$type != "classif" || length(td$class.levels) != 2L)
    stopf("Task must be binary classification!")
  measures = checkMeasures(measures, td)
  assertNumber(mark.th, na.ok = TRUE, lower = 0, upper = 1)
  assertNumber(linesize, lower = 0)

  requirePackages(c("!ggplot2", "reshape2"), why = "plotThreshVsPerf")

  mids = extractSubList(measures, "id")
  # grid for predictions
  thseq = seq(0, 1, length.out = gridsize)
  grid = data.frame(threshold = thseq)
  # eval all perf measures on grid
  perf = asMatrixRows(lapply(thseq, function(threshold) {
    pp = setThreshold(pred, threshold = threshold)
    performance(pp, measures = measures)
  }), col.names = mids)
  grid = cbind(grid, perf)
  grid = reshape2::melt(grid, measure.vars = mids, variable.name = "measure", value.name = "perf")
  p = ggplot(data = grid, mapping = aes_string(x = "threshold", y = "perf", col = "measure"))
  p = p + geom_line(size = linesize)
  if (!is.na(mark.th))
    p = p + geom_vline(xintercept = mark.th)
  return(p)
}
