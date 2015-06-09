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
#' @title Plot learning curve data using ggplot2.
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
#' @title Plot learning curve data using ggvis.
#'
#' @description
#' Visualizes data size (percentage used for model) vs. performance measure(s).
#'
#' @param obj [\code{LearningCurveData}]\cr
#'   Result of \code{\link{generateLearningCurveData}}.
#' @param color_variable [\code{character(1)}]\cr
#'   The variable to be mapped to color in the plot.
#'   Can be "learner" or "measure". If left unspecified
#'   The option with fewer unique values is mapped to color.
#' @param interactive [\code{logical(1)}]\cr
#'   Whether to make the plot interactive with Shiny.
#'   If true then a sidebar menu is created that lets the user
#'   select which measure or learner (whichever is not \code{color_variable}) to display.
#'   Note that if there are multiple learners and multiple measures interactivity is
#'   necessary as ggvis does not currently support facetting or subplots.
#'   If \code{interactive} is true but there are not multiple measures or learners then
#'   the plot will be static.
#'   Default is false.
#' @template ret_ggv
#' @export
plotLearningCurveGGVIS = function(obj, color_variable = NULL, interactive = FALSE) {
  assertClass(obj, "LearningCurveData")
  if (!is.null(color_variable))
    assertChoice(color_variable, c("learner", "measure"))
  plt_data = reshape2::melt(obj, id.vars = c("learner", "perc"), variable.name = "measure", value.name = "perf")
  nmeas = length(unique(plt_data$measure))
  nlearn = length(unique(plt_data$learner))
  if (is.null(color_variable) & nmeas == 1) {
    color_variable = "learner"
    interactive = FALSE
  } else if (is.null(color_variable) & nlearn == 1) {
    color_variable = "measure"
    interactive = FALSE
  } else if (is.null(color_variable) & interactive) {
    if (nlearn > nmeas) {
      color_variable = "measure"
      pick_variable = "learner"
    } else {
      color_variable = "learner"
      pick_variable = "measure"
    }
  } else if (interactive) {
    if (color_variable == "measure")
      pick_variable = "learner"
    else
      pick_variable = "measure"
  } else {
    stop("cannot plot multiple learners and multiple measures statically")
  }

  if (interactive) {
    ui = shiny::shinyUI(
        shiny::pageWithSidebar(
            shiny::headerPanel("learning curve"),
            shiny::sidebarPanel(
                shiny::selectInput("level_variable",
                                   paste("choose a ", pick_variable),
                                   unique(levels(plt_data[[pick_variable]])))
            ),
            shiny::mainPanel(
                shiny::uiOutput("ggvis_ui"),
                ggvis::ggvisOutput("ggvis")
            )
        ))
    server = shiny::shinyServer(function(input, output) {
      plt_data_sub = shiny::reactive(plt_data[which(plt_data[[pick_variable]] == input$level_variable), ])
      plt = ggvis::ggvis(plt_data_sub, ggvis::prop("x", as.name("perc")),
                         ggvis::prop("y", as.name("perf")),
                         ggvis::prop("stroke", as.name(color_variable)))
      plt = ggvis::layer_lines(plt)
      plt = ggvis::layer_points(plt, ggvis::prop("fill", as.name(color_variable)))
      ggvis::bind_shiny(plt, "ggvis", "ggvis_ui")
    })
    shiny::shinyApp(ui, server)
  } else {
    plt = ggvis::ggvis(plt_data, ggvis::prop("x", as.name("perc")),
                       ggvis::prop("y", as.name("perf")),
                       ggvis::prop("stroke", as.name(color_variable)))
    plt = ggvis::layer_lines(plt)
    ggvis::layer_points(plt, ggvis::prop("fill", as.name(color_variable)))
  }
}
