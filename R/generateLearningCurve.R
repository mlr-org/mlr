#' @title Generates a learning curve
#'
#' @description
#' Observe how the performance changes with an increasing number of observations.
#'
#' @param learners [(list of) \code{\link{Learner}}]\cr
#'   Learning algorithms which should be compared.
#' @template arg_task
#' @param resampling [\code{\link{ResampleDesc}} | \code{\link{ResampleInstance}}]\cr
#'   Resampling strategy. If no strategy is given a default "Holdout" will be performed.
#' @param percs [\code{numeric}]\cr
#'   Vector of percentages to be drawn from the training split. Internally \code{\link{makeDownsampleWrapper}} is used. Thus the result will be noisy as the quality of the sample can differ.
#' @param measures [(list of) \code{\link{Measure}}]\cr
#'   Performance measures for the task.
#' @param stratify [\code{logical(1)}]\cr
#'   Only for classification:
#'   Should the downsampled data be stratified according to the target classes?
#' @template arg_showinfo
#' @return [\code{data.frame}]
#' @example
#'  r = generateLearningCurve(list("classif.rpart", "classif.knn"),
#'  task = sonar.task, percs = seq(0.2, 1, by = 0.2),
#'  measures = list(tp, fp, tn, fn), resampling = makeResampleDesc(method = "Subsample", iters = 5),
#'  show.info = FALSE)
#'  print(plotLearningCurve(r))
#' @export
generateLearningCurve = function(learners, task, resampling = NULL,
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
  cbind(learner = learner, perc = perc, perfs)
}

#' @title Plot a learning curve
#' @param x
#'   The result of \code{\link{generateLearningCurve}}.
#' @return ggplot2 plot
#' @seealso \code{\link{generateLearningCurve}}
#' @export
plotLearningCurve = function(x) {
  requirePackages(c("ggplot2", "reshape2"), why = "plotLearningCurve")
  ggdata = melt(x, id.vars = c("learner", "perc"), variable.name = "measure", value.name = "perf")
  ggplot(ggdata, aes_string(x = "perc", y = "perf", colour = "learner")) + layer(geom = "point") +
    layer(geom = "line") + facet_wrap(~measure, scales = "free_y")
}

