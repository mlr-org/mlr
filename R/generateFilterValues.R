#' @title Calculates feature filter values.
#'
#' @description
#' Calculates numerical filter values for features.
#' For a list of features, use \code{\link{listFilterMethods}}.
#'
#' @family generate_plot_data
#' @family filter
#' @aliases FilterValues
#'
#' @template arg_task
#' @param method [\code{character}]\cr
#'   Filter method(s), see above.
#'   Default is \dQuote{rf.importance}.
#' @param nselect [\code{integer(1)}]\cr
#'   Number of scores to request. Scores are getting calculated for all features per default.
#' @param ... [any]\cr
#'   Passed down to selected method.
#' @return [\code{FilterValues}]. A \code{list} containing:
#'   \item{task.desc}{[\code{\link{TaskDesc}}]\cr
#'	   Task description.}
#'   \item{data}{[\code{data.frame}] with columns:
#'     \itemize{
#'       \item \code{name} Name of feature.
#'       \item \code{type} Feature column type.
#'       \item A column for each \code{method} with
#'                   the feature importance values.
#'     }}
#' @export
generateFilterValuesData = function(task, method = "rf.importance", nselect = getTaskNFeats(task), ...) {
  assert(checkClass(task, "ClassifTask"), checkClass(task, "RegrTask"), checkClass(task, "SurvTask"))
  assertSubset(method, choices = ls(.FilterRegister), empty.ok = FALSE)
  td = getTaskDescription(task)
  filter = lapply(method, function(x) .FilterRegister[[x]])
  if (!(any(sapply(filter, function(x) !isScalarNA(filter$pkg)))))
    lapply(filter, function(x) requirePackages(x$pkg, why = "generateFilterValuesData", default.method = "load"))
  check_task = sapply(filter, function(x) td$type %nin% x$supported.tasks)
  if (any(check_task))
    stopf("Filter(s) '%s' not campatible with task of type '%s'",
          paste(method[check_task], collapse = ", "), td$type)

  check_feat = lapply(filter, function(x) setdiff(names(td$nfeat[td$n.feat > 0L]), x$supported.features))
  check_length = sapply(check_feat, length) > 0L
  if (any(check_length)) {
    stopf("Filter(s) '%s' not compatible with features of type '%s' respectively.",
          method[check_length],
          paste(sapply(check_feat[check_length], function(x) paste(x, collapse = ", ")), collapse = ", and"))
  }
  assertCount(nselect)

  fn = getTaskFeatureNames(task)

  fval = lapply(filter, function(x) {
    x = do.call(x$fun, c(list(task = task, nselect = nselect), list(...)))
    missing.score = setdiff(fn, names(x))
    x[missing.score] = NA_real_
    x[match(names(x), fn)]
  })

  fval = do.call(cbind, fval)
  colnames(fval) = method
  types = vcapply(getTaskData(task, target.extra = TRUE)$data[fn], getClass1)
  out = data.frame(name = row.names(fval),
                   type = types,
                   fval, row.names = NULL, stringsAsFactors = FALSE)
  makeS3Obj("FilterValues",
            task.desc = td,
            data = out)
}
#' @export
print.FilterValues = function(x, ...) {
  catf("FilterValues:")
  catf("Task: %s", x$task.desc$id)
  print(head(x$data))
}
#' @title Calculates feature filter values.
#'
#' @family filter
#' @family generate_plot_data
#'
#' @description
#' Calculates numerical filter values for features.
#' For a list of features, use \code{\link{listFilterMethods}}.
#'
#' @template arg_task
#' @param method [\code{character(1)}]\cr
#'   Filter method, see above.
#'   Default is \dQuote{rf.importance}.
#' @param nselect [\code{integer(1)}]\cr
#'   Number of scores to request. Scores are getting calculated for all features per default.
#' @param ... [any]\cr
#'   Passed down to selected method.
#' @return [\code{\link{FilterValues}}].
#' @note \code{getFilterValues} is deprecated in favor of \code{\link{generateFilterValuesData}}.
#' @family filter
#' @export
getFilterValues = function(task, method = "rf.importance", nselect = getTaskNFeats(task), ...) {
  warning("getFilterValues is deprecated. Use generateFilterValuesData.")
  assertChoice(method, choices = ls(.FilterRegister))
  out = generateFilterValuesData(task, method, nselect, ...)
  colnames(out$data)[3] = "val"
  out$data = out$data[, c(1,3,2)]
  makeS3Obj("FilterValues",
            task.desc = out$task.desc,
            method = method,
            data = out$data)
}
#' Plot filter values using ggplot2.
#'
#' @family plot
#' @family filter
#'
#' @param fvalues [\code{\link{FilterValues}}]\cr
#'   Filter values.
#' @param sort [\code{character(1)}]\cr
#'   Sort features like this.
#'   \dQuote{dec} = decreasing, \dQuote{inc} = increasing, \dQuote{none} = no sorting.
#'   Default is decreasing.
#' @param n.show [\code{integer(1)}]\cr
#'   Number of features (maximal) to show.
#'   Default is 20.
#' @param feat.type.cols [\code{logical(1)}]\cr
#'   Colors for factor and numeric features.
#'   \code{FALSE} means no colors.
#'   Default is \code{FALSE}.
#' @template ret_gg2
#' @export
#' @examples
#' fv = generateFilterValuesData(iris.task, method = "chi.squared")
#' plotFilterValues(fv)
plotFilterValues = function(fvalues, sort = "dec", n.show = 20L, feat.type.cols = FALSE) {
  assertClass(fvalues, classes = "FilterValues")
  assertChoice(sort, choices = c("dec", "inc", "none"))
  if (!(is.null(fvalues$method)))
    stop("fvalues must be generated by generateFilterValuesData, not getFilterValues, which is deprecated.")

  n.show = asCount(n.show)

  data = fvalues$data
  methods = colnames(data[, -which(colnames(data) %in% c("name", "type")), drop = FALSE])
  n.show = min(n.show, max(sapply(methods, function(x) sum(!is.na(data[[x]])))))
  data = reshape2::melt(data, id.vars = c("name", "type"), variable = "method")

  if (sort != "none")
    data = do.call(rbind, lapply(methods, function(x)
      head(sortByCol(data[data$method == x, ], "value", (sort == "inc")), n.show)))

  data$name = factor(data$name, levels = as.character(unique(data$name)))
  if (feat.type.cols)
    mp = aes_string(x = "name", y = "value", fill = "type")
  else
    mp = aes_string(x = "name", y = "value")
  plt = ggplot2::ggplot(data = data, mapping = mp)
  plt = plt + ggplot2::geom_bar(position = "identity", stat = "identity")
  if (length(unique(data$method)) > 1L) {
    plt = plt + ggplot2::facet_wrap(~ method, scales = "free_y")
    plt = plt + ggplot2::labs(title = sprintf("%s (%i features)",
                                              fvalues$task.desc$id,
                                              sum(fvalues$task.desc$n.feat)),
                              x = "", y = "")
  } else {
    plt = plt + ggplot2::labs(title = sprintf("%s (%i features), filter = %s",
                                              fvalues$task.desc$id,
                                              sum(fvalues$task.desc$n.feat),
                                              methods),
                              x = "", y = "")
  }
  plt = plt + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
  return(plt)
}
#' Plot filter values using ggvis.
#'
#' @family plot
#' @family filter
#'
#' @param fvalues [\code{\link{FilterValues}}]\cr
#'   Filter values.
#' @param feat.type.cols [\code{logical(1)}]\cr
#'   Colors for factor and numeric features.
#'   \code{FALSE} means no colors.
#'   Default is \code{FALSE}.
#' @template ret_ggv
#' @export
#' @examples \dontrun{
#' fv = generateFilterValuesData(iris.task, method = "chi.squared")
#' plotFilterValuesGGVIS(fv)
#' }
plotFilterValuesGGVIS = function(fvalues, feat.type.cols = FALSE) {
  assertClass(fvalues, classes = "FilterValues")
  if (!(is.null(fvalues$method)))
    stop("fvalues must be generated by generateFilterValuesData, not getFilterValues, which is deprecated.")

  data = fvalues$data
  data = reshape2::melt(data, id.vars = c("name", "type"), variable = "method")

  create_plot = function(data, feat.type.cols) {
    if (feat.type.cols)
      plt = ggvis::ggvis(data, ggvis::prop("x", as.name("name")),
                         ggvis::prop("y", as.name("value")),
                         ggvis::prop("fill", as.name("type")))
    else
      plt = ggvis::ggvis(data, ggvis::prop("x", as.name("name")),
                         ggvis::prop("y", as.name("value")))

    plt = ggvis::layer_bars(plt)
    plt = ggvis::add_axis(plt, "y", title = "")
    plt = ggvis::add_axis(plt, "x", title = "")
    return(plt)
  }

  gen_plot_data = function(data, sort_type, value_column, factor_column, n_show) {
    if (sort_type != "none") {
      data = head(sortByCol(data, "value", FALSE), n = n_show)
      data[[factor_column]] = factor(data[[factor_column]],
                                     levels = data[[factor_column]][order(data[[value_column]],
                                                                          decreasing = sort_type == "decreasing")])
    }
    data
  }

  header = shiny::headerPanel(sprintf("%s (%i features)", fvalues$task.desc$id, sum(fvalues$task.desc$n.feat)))
  method_input = shiny::selectInput("level_variable", "choose a filter method",
                                    unique(levels(data[["method"]])))
  sort_input = shiny::radioButtons("sort_type", "sort features", c("increasing", "decreasing", "none"))
  n_show_input = shiny::numericInput("n_show", "number of features to show",
                                     value = sum(fvalues$task.desc$n.feat),
                                     min = 1,
                                     max = sum(fvalues$task.desc$n.feat),
                                     step = 1)
  ui = shiny::shinyUI(
    shiny::pageWithSidebar(
      header,
      shiny::sidebarPanel(method_input, sort_input, n_show_input),
      shiny::mainPanel(shiny::uiOutput("ggvis_ui"), ggvis::ggvisOutput("ggvis"))
    )
  )
  server = shiny::shinyServer(function(input, output) {
    plt = shiny::reactive(
      create_plot(
        data = gen_plot_data(
          data[which(data[["method"]] == input$level_variable), ],
          input$sort_type,
          "value",
          "name",
          input$n_show
        ),
        feat.type.cols
      )
    )
    ggvis::bind_shiny(plt, "ggvis", "ggvis_ui")
  })
  shiny::shinyApp(ui, server)
}
