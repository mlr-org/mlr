#' @title Generates a learning curve
#'
#' @description
#' Observe how the performance changes with an increasing number of observations.
#'
#' @param learners [(list of) \code{\link{Learner}}]\cr
#'   Learning algorithms which should be compared.
#' @template arg_task
#' @param resampling [\code{\link{ResampleDesc}} | \code{\link{ResampleInstance}}]\cr
#'   Resampling strategy to evaluate the performance measure.
#'   If no strategy is given a default "Holdout" will be performed.
#' @param percs [\code{numeric}]\cr
#'   Vector of percentages to be drawn from the training split.
#'   These values represent the x-axis.
#'   Internally \code{\link{makeDownsampleWrapper}} is used in combination with \code{\link{benchmark}}.
#'   Thus for each percentage a different set of observations is drawn resulting in noisy performance measures as the quality of the sample can differ.
#' @param measures [(list of) \code{\link{Measure}}]\cr
#'   Performance measures to generate learning curves for, representing the y-axis.
#' @param stratify [\code{logical(1)}]\cr
#'   Only for classification:
#'   Should the downsampled data be stratified according to the target classes?
#' @template arg_showinfo
#' @return A [\code{data.frame}] of class \code{LearningCurveData}.
#' @examples
#' r = generateLearningCurveData(list("classif.rpart", "classif.knn"),
#' task = sonar.task, percs = seq(0.2, 1, by = 0.2),
#' measures = list(tp, fp, tn, fn), resampling = makeResampleDesc(method = "Subsample", iters = 5),
#' show.info = FALSE)
#' print(plotLearningCurve(r))
#' @export
generateLearningCurveData = function(learners, task, resampling = NULL,
  percs = seq(0.1, 1, by = 0.1), measures, stratify = FALSE, show.info = getMlrOption("show.info"))  {

  learners = lapply(learners, checkLearner)
  assertClass(task, "Task")
  assertNumeric(percs, lower = 0L, upper = 1L, min.len = 2L, any.missing = FALSE)
  measures = checkMeasures(measures, task)
  assertFlag(stratify)

  if (is.null(resampling))
    resampling = makeResampleInstance("Holdout", task = task)
  else
    assert(checkClass(resampling, "ResampleDesc"), checkClass(resampling, "ResampleInstance"))

  perc.ids = seq_along(percs)

  # create downsampled versions for all learners
  lrnds1 = lapply(learners, function(lrn) {
    lrn.downsampleds = lapply(perc.ids, function(p.id) {
      perc = percs[p.id]
      dsw = makeDownsampleWrapper(learner = lrn, dw.perc = perc, dw.stratify = stratify)
      list(
        lrn.id = lrn$id,
        lrn = setId(dsw, paste0(lrn$id, ".", p.id)),
        perc = perc
      )
    })
  })
  lrnds2 = unlist(lrnds1, recursive = FALSE)
  dsws = extractSubList(lrnds2, "lrn", simplify = FALSE)

  bench.res = benchmark(dsws, task, resampling,  measures, show.info = show.info)
  perfs = getBMRAggrPerformances(bench.res, as.df = TRUE)

  # get perc and learner col data
  perc = extractSubList(lrnds2[perfs$learner.id], "perc")
  learner = extractSubList(lrnds2[perfs$learner.id], "lrn.id")
  perfs = dropNamed(perfs, c("task.id", "learner.id"))

  # set short measures names and resort cols
  mids = extractSubList(measures, "id")
  colnames(perfs) = mids
  out = cbind(learner = learner, perc = perc, perfs)
  class(out) = append(class(out), "LearningCurveData")
  out
}
#' @title Plot learning curve data.
#'
#' @description
#' Visualizes data size (percentage used for model) vs. performance measure(s).
#'
#' @param obj [\code{LearningCurveData}]\cr
#'   Result of \code{\link{generateLearningCurveData}}, with class \code{LearningCurveData}.
#' @template ret_gg2
#' @export
plotLearningCurve = function(obj) {
  assertClass(obj, "LearningCurveData")
  ggdata = reshape2::melt(obj, id.vars = c("learner", "perc"), variable.name = "measure", value.name = "perf")
  pl = ggplot2::ggplot(ggdata, ggplot2::aes_string(x = "perc", y = "perf", colour = "learner"))
  pl = pl + ggplot2::layer(geom = "point")
  pl = pl + ggplot2::layer(geom = "line")
  pl = pl + ggplot2::facet_wrap(~measure, scales = "free_y")
  return(pl)
}
