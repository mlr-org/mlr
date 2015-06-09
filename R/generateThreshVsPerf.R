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
  obj = obj$pred
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
            measures = mids,
            data = out)
}
#' @title Plot threshold vs. performance(s) for 2-class classification using ggplot2.
#'
#' @family plot
#' @family thresh_vs_perf
#'
#' @param obj [\code{ThreshVsPerfData}]\cr
#'   Result of \code{\link{generateThreshVsPerfData}}.
#' @param mark.th [\code{numeric(1)}]\cr
#'   Mark given threshold with vertical line?
#'   Default is \code{NA} which means not to do it.
#' @template ret_gg2
#' @export
#' @examples \dontrun{
#' lrn = makeLearner("classif.rpart", predict.type = "prob")
#' mod = train(lrn, sonar.task)
#' pred = predict(mod, sonar.task)
#' pvs = generateThreshVsPerfData(pred, list(tpr, fpr))
#' plotThreshVsPerf(pvs)
#' }
plotThreshVsPerf = function(obj, mark.th = NA_real_) {
  assertClass(obj, classes = "ThreshVsPerfData")
  data = reshape2::melt(obj$data, measure.vars = obj$measures,
              variable.name = "measure", value.name = "perf",
              id.vars = c("learner", "threshold"))
  plt = ggplot2::ggplot(data, aes_string(x = "threshold", y = "perf", col = "measure"))
  plt = plt + ggplot2::geom_line()
  if (!is.na(mark.th))
    plt = plt + ggplot2::geom_vline(xintercept = mark.th)
  if (length(unique(data$learner)) > 1L)
    plt = plt + ggplot2::facet_wrap(~ learner, scales = "free_y")
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
#' @param interactive [\code{logical(1)}]\cr
#'   Whether to make the plot interactive with Shiny.
#'   If \code{TRUE} then a sidebar menu is created that lets the user
#'   select which learner to display.
#'   Note that if there are multiple learners and multiple measures interactivity is
#'   necessary as ggvis does not currently support facetting or subplots.
#'   If \code{interactive} is \code{TRUE} but there are not multiple measures or learners then
#'   the plot will be static.
#'   Note that \code{mark.th} is not supported when \code{interactive = TRUE}.
#'   Default is \code{FALSE}.
#' @template ret_ggv
#' @export
#' @examples \dontrun{
#' lrn = makeLearner("classif.rpart", predict.type = "prob")
#' mod = train(lrn, sonar.task)
#' pred = predict(mod, sonar.task)
#' pvs = generateThreshVsPerfData(pred, list(tpr, fpr))
#' plotThreshVsPerfGGVIS(pvs)
#' }
plotThreshVsPerfGGVIS = function(obj, mark.th = NA_real_, interactive = FALSE) {
  assertClass(obj, classes = "ThreshVsPerfData")
  data = reshape2::melt(obj$data, measure.vars = obj$measures,
                        variable.name = "measure", value.name = "perf",
                        id.vars = c("learner", "threshold"))

  create_plot = function(data, measures) {
    if (length(obj$measures) > 1L) {
      plt = ggvis::ggvis(data, ggvis::prop("x", as.name("threshold")),
                         ggvis::prop("y", as.name("perf")),
                         ggvis::prop("stroke", as.name("measure")))
    } else {
      plt = ggvis::ggvis(data, ggvis::prop("x", as.name("threshold")),
                         ggvis::prop("y", as.name("perf")))
    }
    plt = ggvis::layer_lines(plt)
    if (!is.na(mark.th) & !interactive) { ## cannot do vline with reactive data
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
  
  if (interactive & length(unique(data$learner)) > 1L) {
    ui = shiny::shinyUI(
        shiny::pageWithSidebar(
            shiny::headerPanel(""),
            shiny::sidebarPanel(
                shiny::selectInput("learner_select",
                                   "choose a learner",
                                   levels(data[["learner"]]))
            ),
            shiny::mainPanel(
                shiny::uiOutput("ggvis_ui"),
                ggvis::ggvisOutput("ggvis")
            )
        ))
    server = shiny::shinyServer(function(input, output) {
      data_sub = shiny::reactive(data[which(data[["learner"]] == input$learner_select), ])
      plt = create_plot(data_sub, obj$measures)
      ggvis::bind_shiny(plt, "ggvis", "ggvis_ui")
    })
    shiny::shinyApp(ui, server)
  } else {
    create_plot(data, obj$measures)
  }
}
