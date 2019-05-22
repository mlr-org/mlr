#' @title Generates a learning curve.
#'
#' @description
#' Observe how the performance changes with an increasing number of observations.
#'
#' @family generate_plot_data
#' @family learning_curve
#' @aliases LearningCurveData
#'
#' @param learners [(list of) [Learner])\cr
#'   Learning algorithms which should be compared.
#' @template arg_task
#' @param resampling ([ResampleDesc] | [ResampleInstance])\cr
#'   Resampling strategy to evaluate the performance measure.
#'   If no strategy is given a default "Holdout" will be performed.
#' @param percs ([numeric])\cr
#'   Vector of percentages to be drawn from the training split.
#'   These values represent the x-axis.
#'   Internally [makeDownsampleWrapper] is used in combination with [benchmark].
#'   Thus for each percentage a different set of observations is drawn resulting in noisy performance measures as the quality of the sample can differ.
#' @param measures [(list of) [Measure])\cr
#'   Performance measures to generate learning curves for, representing the y-axis.
#' @param stratify (`logical(1)`)\cr
#'   Only for classification:
#'   Should the downsampled data be stratified according to the target classes?
#' @template arg_showinfo
#' @return ([LearningCurveData]). A `list` containing:
#'   \item{task}{[[Task])\cr
#'     The task.}
#'   \item{measures}{[(list of) [Measure])\cr
#'     Performance measures.}
#'   \item{data}{([data.frame]) with columns:
#'     \itemize{
#'       \item `learner` Names of learners.
#'       \item `percentage` Percentages drawn from the training split.
#'       \item One column for each
#'     [Measure] passed to [generateLearningCurveData].
#'    }}
#' @examples
#' r = generateLearningCurveData(list("classif.rpart", "classif.knn"),
#'   task = sonar.task, percs = seq(0.2, 1, by = 0.2),
#'   measures = list(tp, fp, tn, fn), resampling = makeResampleDesc(method = "Subsample", iters = 5),
#'   show.info = FALSE)
#' plotLearningCurve(r)
#' @noMd
#' @export
generateLearningCurveData = function(learners, task, resampling = NULL,
  percs = seq(0.1, 1, by = 0.1), measures, stratify = FALSE, show.info = getMlrOption("show.info")) {

  learners = ensureVector(learners, 1, "Learner")
  learners = lapply(learners, checkLearner)
  assertClass(task, "Task")
  assertNumeric(percs, lower = 0L, upper = 1L, min.len = 2L, any.missing = FALSE)
  measures = checkMeasures(measures, task)
  assertFlag(stratify)

  if (is.null(resampling)) {
    resampling = makeResampleInstance("Holdout", task = task)
  } else {
    assert(checkClass(resampling, "ResampleDesc"), checkClass(resampling, "ResampleInstance"))
  }

  # create downsampled versions for all learners
  lrnds1 = lapply(learners, function(lrn) {
    lapply(seq_along(percs), function(p.id) {
      perc = percs[p.id]
      dsw = makeDownsampleWrapper(learner = lrn, dw.perc = perc, dw.stratify = stratify)
      list(
        lrn.id = lrn$id,
        lrn = setLearnerId(dsw, stri_paste(lrn$id, ".", p.id)),
        perc = perc
      )
    })
  })
  lrnds2 = unlist(lrnds1, recursive = FALSE)
  dsws = extractSubList(lrnds2, "lrn", simplify = FALSE)

  bench.res = benchmark(dsws, task, resampling, measures, show.info = show.info)
  perfs = getBMRAggrPerformances(bench.res, as.df = TRUE)

  # get perc and learner col data
  perc = extractSubList(lrnds2[perfs$learner.id], "perc")
  learner = extractSubList(lrnds2[perfs$learner.id], "lrn.id")
  perfs = dropNamed(perfs, c("task.id", "learner.id"))

  # set short measures names and resort cols
  mids = replaceDupeMeasureNames(measures, "id")
  names(measures) = mids
  colnames(perfs) = mids
  out = cbind(learner = learner, percentage = perc, perfs)
  makeS3Obj("LearningCurveData",
    task = task,
    measures = measures,
    data = out)
}
#' @export
print.LearningCurveData = function(x, ...) {
  catf("LearningCurveData:")
  catf("Task: %s", x$task$task.desc$id)
  catf("Measures: %s", collapse(extractSubList(x$measures, "name")))
  printHead(x$data, ...)
}
#' @title Plot learning curve data using ggplot2.
#'
#' @family learning_curve
#' @family plot
#'
#' @description
#' Visualizes data size (percentage used for model) vs. performance measure(s).
#'
#' @param obj ([LearningCurveData])\cr
#'   Result of [generateLearningCurveData], with class `LearningCurveData`.
#' @param facet (`character(1)`)\cr
#'   Selects \dQuote{measure} or \dQuote{learner} to be the facetting variable.
#'   The variable mapped to `facet` must have more than one unique value, otherwise it will
#'   be ignored. The variable not chosen is mapped to color if it has more than one unique value.
#'   The default is \dQuote{measure}.
#' @param pretty.names (`logical(1)`)\cr
#'   Whether to use the [Measure] name instead of the id in the plot.
#'   Default is `TRUE`.
#' @template arg_facet_nrow_ncol
#' @template ret_gg2
#' @export
plotLearningCurve = function(obj, facet = "measure", pretty.names = TRUE,
  facet.wrap.nrow = NULL, facet.wrap.ncol = NULL) {

  assertClass(obj, "LearningCurveData")
  mappings = c("measure", "learner")
  assertChoice(facet, mappings)
  assertFlag(pretty.names)
  color = mappings[mappings != facet]

  if (pretty.names) {
    mnames = replaceDupeMeasureNames(obj$measures, "name")
    colnames(obj$data) = mapValues(colnames(obj$data),
      names(obj$measures), mnames)
  }

  data = melt(as.data.table(obj$data), id.vars = c("learner", "percentage"), variable.name = "measure", value.name = "performance")
  nlearn = length(unique(data$learner))
  nmeas = length(unique(data$measure))

  if ((color == "learner" & nlearn == 1L) | (color == "measure" & nmeas == 1L)) {
    color = NULL
  }
  if ((facet == "learner" & nlearn == 1L) | (facet == "measure" & nmeas == 1L)) {
    facet = NULL
  }

  if (!is.null(color)) {
    plt = ggplot(data, aes_string(x = "percentage", y = "performance", colour = color))
  } else {
    plt = ggplot(data, aes_string(x = "percentage", y = "performance"))
  }
  plt = plt + geom_point()
  plt = plt + geom_line()
  if (!is.null(facet)) {
    plt = plt + ggplot2::facet_wrap(as.formula(stri_paste("~", facet, sep = " ")),
      scales = "free_y", nrow = facet.wrap.nrow, ncol = facet.wrap.ncol)
  }
  return(plt)
}
