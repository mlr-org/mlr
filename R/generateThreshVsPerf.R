#' @title Generate threshold vs. performance(s) for 2-class classification.
#'
#' @family generate_plot_data
#' @family thresh_vs_perf
#'
#' @template arg_plotroc_obj
#' @template arg_measures
#' @param gridsize [\code{integer(1)}]\cr
#'   Grid resolution for x-axis (threshold).
#'   Default is 100.
#' @param task.id [\code{character(1)}]\cr
#'   Selected task in \code{\link{BenchmarkResult}} to do plots for, ignored otherwise.
#'   Default is first task.
#' @export
generateThreshVsPerfData = function(obj, measures, gridsize = 100L, task.id = NULL)
  UseMethod("generateThreshVsPerfData")
#' @export
generateThreshVsPerfData.Prediction = function(obj, measures, gridsize = 100L, task.id = NULL)
  generateThreshVsPerfData.list(namedList("prediction", obj), measures, gridsize, task.id)
#' @export
generateThreshVsPerfData.ResampleResult = function(obj, measures, gridsize = 100L, task.id = NULL) {
  obj = getRRPredictions(obj)
  assertClass(obj, "Prediction")
  assert(obj$predict.type == "prob")
  generateThreshVsPerfData.Prediction(obj, measures, gridsize)
}
#' @export
generateThreshVsPerfData.BenchmarkResult = function(obj, measures, gridsize = 100L, task.id = NULL) {
  tids = getBMRTaskIds(obj)
  if (is.null(task.id))
    task.id = tids[1L]
  else
    assertChoice(task.id, tids)
  obj = getBMRPredictions(obj, task.ids = task.id, as.df = FALSE)[[1L]]
  assert(all(extractSubList(obj, "predict.type") == "prob"))
  generateThreshVsPerfData.list(obj, measures, gridsize, task.id)
}
#' @export
generateThreshVsPerfData.list = function(obj, measures, gridsize = 100L, task.id = NULL) {
  assertList(obj, c("Prediction", "ResampleResult"), min.len = 1L)
  ## unwrap ResampleResult to Prediction and set default names
  if (inherits(obj[[1L]], "ResampleResult")) {
    if (is.null(names(obj)))
      names(obj) = extractSubList(obj, c("pred", "learner.id"))
    obj = extractSubList(obj, "pred", simplify = FALSE)
  }
  td = BBmisc::extractSubList(obj, "task.desc", simplify = FALSE)[[1L]]
  measures = checkMeasures(measures, td)
  mids = extractSubList(measures, "id")
  if (td$type != "classif" || length(td$class.levels) != 2L)
    stopf("Task must be binary classification!")
  assertList(obj, names = "unique")
  thseq = seq(0, 1, length.out = gridsize)
  grid = data.frame(threshold = thseq)
  obj = lapply(obj, function(x) {
    assertClass(x, "Prediction")
    assert(x$predict.type == "prob")
    asMatrixRows(lapply(thseq, function(threshold) {
      pp = setThreshold(x, threshold = threshold)
      performance(pp, measures = measures)
    }), col.names = mids)
  })
  out = plyr::ldply(obj, .id = "learner")
  out = cbind(grid, out)
  makeS3Obj("ThreshVsPerfData",
            measures = measures,
            data = out)
}
#' @title Plot threshold vs. performance(s) for 2-class classification using ggplot2.
#'
#' @family plot
#' @family thresh_vs_perf
#'
#' @param obj [\code{ThreshVsPerfData}]\cr
#'   Result of \code{\link{generateThreshVsPerfData}}.
#' @param facet [\code{character(1)}]\cr
#'   Selects \dQuote{measure} or \dQuote{learner} to be the facetting variable.
#'   The variable mapped to \code{facet} must have more than one unique value, otherwise it will
#'   be ignored. The variable not chosen is mapped to color if it has more than one unique value.
#'   The default is \dQuote{measure}.
#' @param mark.th [\code{numeric(1)}]\cr
#'   Mark given threshold with vertical line?
#'   Default is \code{NA} which means not to do it.
#' @template ret_gg2
#' @export
#' @examples
#' lrn = makeLearner("classif.rpart", predict.type = "prob")
#' mod = train(lrn, sonar.task)
#' pred = predict(mod, sonar.task)
#' pvs = generateThreshVsPerfData(pred, list(tpr, fpr))
#' plotThreshVsPerf(pvs)
plotThreshVsPerf = function(obj, facet = "measure", mark.th = NA_real_) {
  assertClass(obj, classes = "ThreshVsPerfData")
  mappings = c("measure", "learner")
  assertChoice(facet, mappings)
  color = mappings[mappings != facet]

  colnames(obj$data) = BBmisc::mapValues(colnames(obj$data), extractSubList(obj$measures, "id"),
                                         extractSubList(obj$measures, "name"))

  data = reshape2::melt(obj$data, measure.vars = extractSubList(obj$measures, "name"),
              variable.name = "measure", value.name = "perf",
              id.vars = c("learner", "threshold"))
  nlearn = length(unique(data$learner))
  nmeas = length(unique(data$measure))

  if ((color == "learner" & nlearn == 1L) | (color == "measure" & nmeas == 1L))
    color = NULL
  if ((facet == "learner" & nlearn == 1L) | (facet == "measure" & nmeas == 1L))
    facet = NULL

  if (!is.null(color))
    plt = ggplot2::ggplot(data, aes_string(x = "threshold", y = "perf", color = color))
  else
    plt = ggplot2::ggplot(data, aes_string(x = "threshold", y = "perf"))
  plt = plt + ggplot2::geom_line()
  if (!is.na(mark.th))
    plt = plt + ggplot2::geom_vline(xintercept = mark.th)
  if (!is.null(facet))
    plt = plt + ggplot2::facet_wrap(as.formula(paste("~", facet)), scales = "free_y")
  return(plt)
}
#' @title Plot threshold vs. performance(s) for 2-class classification using ggvis.
#'
#' @family plot
#' @family thresh_vs_perf
#'
#' @param obj [\code{ThreshVsPerfData}]\cr
#'   Result of \code{\link{generateThreshVsPerfData}}.
#' @param mark.th [\code{numeric(1)}]\cr
#'   Mark given threshold with vertical line?
#'   Default is \code{NA} which means not to do it.
#' @param interaction [\code{character(1)}]\cr
#'   Selects \dQuote{measure} or \dQuote{learner} to be used in a Shiny application
#'   making the \code{interaction} variable selectable via a drop-down menu.
#'   This variable must have more than one unique value, otherwise it will be ignored.
#'   The variable not chosen is mapped to color if it has more than one unique value.
#'   Note that if there are multiple learners and multiple measures interactivity is
#'   necessary as ggvis does not currently support facetting or subplots.
#'   The default is \dQuote{measure}.
#' @template ret_ggv
#' @export
#' @examples \dontrun{
#' lrn = makeLearner("classif.rpart", predict.type = "prob")
#' mod = train(lrn, sonar.task)
#' pred = predict(mod, sonar.task)
#' pvs = generateThreshVsPerfData(pred, list(tpr, fpr))
#' plotThreshVsPerfGGVIS(pvs)
#' }
plotThreshVsPerfGGVIS = function(obj, interaction = "measure",
                                 mark.th = NA_real_) {
  assertClass(obj, classes = "ThreshVsPerfData")
  mappings = c("measure", "learner")
  assertChoice(interaction, mappings)
  color = mappings[mappings != interaction]

  colnames(obj$data) = BBmisc::mapValues(colnames(obj$data), extractSubList(obj$measures, "id"),
                                         extractSubList(obj$measures, "name"))

  data = reshape2::melt(obj$data, measure.vars = extractSubList(obj$measures, "name"),
                        variable.name = "measure", value.name = "perf",
                        id.vars = c("learner", "threshold"))
  nmeas = length(unique(data$measure))
  nlearn = length(unique(data$learner))

  if ((color == "learner" & nlearn == 1L) | (color == "measure" & nmeas == 1L))
    color = NULL
  if ((interaction == "learner" & nlearn == 1L) | (interaction == "measure" & nmeas == 1L))
    interaction = NULL

  create_plot = function(data, color, measures) {
    if (!is.null(color)) {
      plt = ggvis::ggvis(data, ggvis::prop("x", as.name("threshold")),
                         ggvis::prop("y", as.name("perf")),
                         ggvis::prop("stroke", as.name(color)))
    } else {
      plt = ggvis::ggvis(data, ggvis::prop("x", as.name("threshold")),
                         ggvis::prop("y", as.name("perf")))
    }
    plt = ggvis::layer_lines(plt)
    if (!is.na(mark.th) & is.null(interaction)) { ## cannot do vline with reactive data
      vline_data = data.frame(x2 = rep(mark.th, 2), y2 = c(min(data$perf), max(data$perf)),
                              measure = obj$measures[1])
      plt = ggvis::layer_paths(plt, ggvis::prop("x", as.name("x2")),
                               ggvis::prop("y", as.name("y2")),
                               ggvis::prop("stroke", "grey", scale = FALSE), data = vline_data)
    }
    plt = ggvis::add_axis(plt, "x", title = "threshold")
    if (length(measures) > 1L)
      plt = ggvis::add_axis(plt, "y", title = "measure")
    else
      plt = ggvis::add_axis(plt, "y", title = measures[1])
    plt
  }

  if (!is.null(interaction)) {
    ui = shiny::shinyUI(
        shiny::pageWithSidebar(
            shiny::headerPanel("Threshold vs. Performance"),
            shiny::sidebarPanel(
                shiny::selectInput("interaction_select",
                                   paste("choose a", interaction),
                                   levels(data[[interaction]]))
            ),
            shiny::mainPanel(
                shiny::uiOutput("ggvis_ui"),
                ggvis::ggvisOutput("ggvis")
            )
        ))
    server = shiny::shinyServer(function(input, output) {
      data_sub = shiny::reactive(data[which(data[[interaction]] == input$interaction_select), ])
      plt = create_plot(data_sub, color, obj$measures)
      ggvis::bind_shiny(plt, "ggvis", "ggvis_ui")
    })
    shiny::shinyApp(ui, server)
  } else {
    create_plot(data, color, obj$measures)
  }
}
