#' @title Plot threshold vs. performance(s) for 2-class classification using ggplot2.
#'
#' @template arg_pred
#' @template arg_measures
#' @param mark.th [\code{numeric(1)}]\cr
#'   Mark given threshold with vertical line?
#'   Default is \code{NA} which means not to do it.
#' @param gridsize [\code{integer(1)}]\cr
#'   Grid resolution for x-axis (threshold).
#'   Default is 100.
#' @template ret_gg2
#' @export
plotThreshVsPerf = function(pred, measures, mark.th = NA_real_, gridsize = 100L) {
  assertClass(pred, classes = "Prediction")
  td = pred$task.desc
  if (td$type != "classif" || length(td$class.levels) != 2L)
    stopf("Task must be binary classification!")
  measures = checkMeasures(measures, td)
  assertNumber(mark.th, na.ok = TRUE, lower = 0, upper = 1)

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
  grid = melt(grid, measure.vars = mids, variable.name = "measure", value.name = "perf")
  p = ggplot(data = grid, mapping = aes_string(x = "threshold", y = "perf", col = "measure"))
  p = p + geom_line()
  if (!is.na(mark.th))
    p = p + geom_vline(xintercept = mark.th)
  return(p)
}
#' @title Plot threshold vs. performance(s) for 2-class classification using ggvis.
#'
#' @template arg_pred
#' @template arg_measures
#' @param mark.th [\code{numeric(1)}]\cr
#'   Mark given threshold with vertical line?
#'   Default is \code{NA} which means not to do it.
#' @param gridsize [\code{integer(1)}]\cr
#'   Grid resolution for x-axis (threshold).
#'   Default is 100.
#' @template ret_ggv
#' @export
#' @examples \dontrun{
#' lrn = makeLearner("classif.rpart", predict.type = "prob")
#' mod = train(lrn, sonar.task)
#' pred = predict(mod, sonar.task)
#' plotThreshVsPerf(pred, list(tpr, fpr))
#' }
plotThreshVsPerfGGVIS = function(pred, measures, mark.th = NA_real_, gridsize = 100L) {
  assertClass(pred, classes = "Prediction")
  td = pred$task.desc
  if (td$type != "classif" || length(td$class.levels) != 2L)
    stopf("Task must be binary classification!")
  measures = checkMeasures(measures, td)
  assertNumber(mark.th, na.ok = TRUE, lower = 0, upper = 1)

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
  plt_data = reshape2::melt(grid, measure.vars = mids, variable.name = "measure", value.name = "perf")

  if (is.list(measures) & length(measures) > 1L) {
    plt = ggvis::ggvis(plt_data, ggvis::prop("x", as.name("threshold")),
                       ggvis::prop("y", as.name("perf")),
                       ggvis::prop("stroke", as.name("measure")))
  } else {
    plt = ggvis::ggvis(plt_data, ggvis::prop("x", as.name("threshold")),
                       ggvis::prop("y", as.name("perf")))
  }

  plt = ggvis::layer_lines(plt)

  if (!is.na(mark.th)) {
    vline_data = data.frame(x2 = rep(mark.th, 2), y2 = c(min(plt_data$perf), max(plt_data$perf)),
                            measure = plt_data$measure[1])
    plt = ggvis::layer_paths(plt, ggvis::prop("x", as.name("x2")),
                             ggvis::prop("y", as.name("y2")),
                             ggvis::prop("stroke", "grey", scale = FALSE), data = vline_data)
  }

  plt = ggvis::add_axis(plt, "x", title = "threshold")

  if (is.list(measures) & length(measures) > 1L)
    plt = ggvis::add_axis(plt, "y", title = "measure")
  else
    plt = ggvis::add_axis(plt, "y", title = as.character(plt_data$measure[1]))

  return(plt)
}
