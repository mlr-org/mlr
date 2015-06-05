#' Plot filter values using ggplot2.
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
#' @param feat.type.cols [\code{character(2)}*]\cr
#'   Colors for factor and numeric features.
#'   \code{NULL} means no colors.
#'   Default is darkgreen and darkblue.
#' @template ret_gg2
#' @export
#' @examples
#' fv = getFilterValues(iris.task, method = "chi.squared")
#' plotFilterValues(fv)
plotFilterValues = function(fvalues, sort = "dec", n.show = 20L, feat.type.cols = c("darkgreen", "darkblue")) {
  assertClass(fvalues, classes = "FilterValues")
  assertChoice(sort, choices = c("dec", "inc", "none"))
  n.show = asCount(n.show)
  data = lapply(fvalues$data, function(data) {
    n.show = min(n.show, sum(!is.na(data$val)))
    if (sort != "none")
      data = head(sortByCol(data, "val", asc = (sort == "inc")), n.show)
    data$name = factor(data$name, levels = as.character(data$name))
    data
  })
  data = plyr::ldply(data, .id = "method")

  if (!is.null(feat.type.cols)) {
    assertCharacter(feat.type.cols, len = 2L, any.missing = FALSE)
    mp = aes_string(x = "name", y = "val", fill = "type")
  } else {
    mp = aes_string(x = "name", y = "val")
  }
  p = ggplot(data = data, mapping = mp)
  p = p + geom_bar(position = "identity", stat = "identity")
  if (!is.null(feat.type.cols))
    p = p + scale_fill_manual(values = feat.type.cols)
  if (length(unique(data$method)) == 1L) {
    p = p + ggtitle(sprintf("%s (%i features), filter = %s",
                            fvalues$task.desc$id, sum(fvalues$task.desc$n.feat), fvalues$method))
  } else {
    p = p + ggtitle(sprintf("%s (%i features)", fvalues$task.desc$id, sum(fvalues$task.desc$n.feat)))
    p = p + facet_wrap(~ method, scales = "free_y")
  }
  p = p + xlab("") + ylab("")
  p = p + theme(axis.text.x = element_text(angle = 45, hjust = 1))
  return(p)
}
#' Plot filter values using ggvis.
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
#' @param feat.type.cols [\code{character(2)}*]\cr
#'   Colors for factor and numeric features.
#'   \code{NULL} means no colors.
#'   Default is darkgreen and darkblue.
#' @param interactive [\code{logical(1)}]\cr
#'   Whether to use Shiny to visualize multiple filter methods.
#'   If \code{TRUE} and \code{getFilterValues} was called with multiple methods,
#'   then returns a Shiny application with a drop-down menu allowing the visualization
#'   of said methods. If \code{FALSE} then a static \code{ggvis} plot is returned.
#'   Default is \code{FALSE}.
#' @template ret_ggv
#' @export
#' @examples
#' \dontrun{
#' fv = getFilterValues(iris.task, method = "chi.squared")
#' plotFilterValuesGGVIS(fv)
#'
#' fvm = getFilterValues(iris.task, method = c("chi.squared", "rf.importance"))
#' plotFilterValuesGGVIS(fvm, interactive = TRUE)
#' }
plotFilterValuesGGVIS = function(fvalues, sort = "dec", n.show = 20L, feat.type.cols = c("darkgreen", "darkblue"),
                                 interactive = FALSE) {
  assertClass(fvalues, classes = "FilterValues")
  assertChoice(sort, choices = c("dec", "inc", "none"))
  n.show = asCount(n.show)
  data = lapply(fvalues$data, function(data) {
    n.show = min(n.show, sum(!is.na(data$val)))
    if (sort != "none")
      data = head(sortByCol(data, "val", asc = (sort == "inc")), n.show)
    data$name = factor(data$name, levels = as.character(data$name))
    data
  })
  data = plyr::ldply(data, .id = "method")

  create_plot <- function(data, fvalues, feat.type.cols = NULL, method_name) {
    if (!is.null(feat.type.cols)) {
      assertCharacter(feat.type.cols, len = 2L, any.missing = FALSE)
      p = ggvis::ggvis(data, ggvis::prop("x", as.name("name")),
                       ggvis::prop("y", as.name("val")),
                       ggvis::prop("fill", as.name("type")))
      p = ggvis::layer_bars(p)
      p = ggvis::scale_nominal(p, "fill", range = feat.type.cols)
    } else {
      p = ggvis::ggvis(data, ggvis::prop("x", as.name("name")),
                       ggvis::prop("y", as.name("val")))
      p = ggvis::layer_bars(p)
    }
    p = ggvis::add_axis(p, "y", title = "")
    return(p)
  }

  add_title <- function(vis, ..., x_lab = "", title = "") {
      vis = ggvis::add_axis(vis, "x", title = x_lab)
      vis = ggvis::add_axis(vis, "x", orient = "top", ticks = 0, title = title,
                     properties = ggvis::axis_props(
                         axis = list(stroke = "white"),
                         labels = list(fontSize = 0)
                     ), ...)
  }

  if (length(unique(data$method)) == 1L) {
    p = create_plot(data, fvalues, feat.type.cols)
    p = add_title(p, x_lab = "",
                  title = sprintf("%s (%i features), filter = %s",
                                  fvalues$task.desc$id,
                                  sum(fvalues$task.desc$n.feat),
                                  unique(data$method)))
    return(p)
  } else if (length(unique(data$method)) > 1L & interactive) {
    ui = shiny::shinyUI(
        shiny::pageWithSidebar(
            shiny::headerPanel(sprintf("%s (%i features)", fvalues$task.desc$id, sum(fvalues$task.desc$n.feat))),
            shiny::sidebarPanel(
                shiny::selectInput("level_variable",
                                   "choose a filter method",
                                   unique(levels(data[["method"]])))
            ),
            shiny::mainPanel(
                shiny::uiOutput("ggvis_ui"),
                ggvis::ggvisOutput("ggvis")
            )
        ))
    server = shiny::shinyServer(function(input, output) {
      data_sub = shiny::reactive(data[which(data[["method"]] == input$level_variable), ])
      p = create_plot(data_sub, fvalues, feat.type.cols)
      ggvis::bind_shiny(p, "ggvis", "ggvis_ui")
    })
    shiny::shinyApp(ui, server)
  } else {
    stop("If multiple filter methods are passed to getFilterValues interactive must be set to TRUE.")
  }
}
