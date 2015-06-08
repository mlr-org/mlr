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
#' @param feat.type.cols [\code{logical(1)}]\cr
#'   Color factor and numeric features.
#'   \code{FALSE} means no colors.
#'   Default is \code{FALSE}.
#' @template ret_gg2
#' @export
#' @examples
#' fv = getFilterValues(iris.task, method = "chi.squared")
#' plotFilterValues(fv)
plotFilterValues = function(fvalues, sort = "dec", n.show = 20L, feat.type.cols = FALSE) {
  assertClass(fvalues, classes = "FilterValues")
  assertChoice(sort, choices = c("dec", "inc", "none"))
  assertFlag(feat.type.cols)
  n.show = asCount(n.show)

  data = fvalues$data
  n.show = min(n.show, sum(!is.na(data$val)))
  if (sort != "none")
    data = head(sortByCol(data, "val", asc = (sort == "inc")), n.show)

  data$name = factor(data$name, levels = as.character(data$name))
  if (feat.type.cols)
    mp = ggplot2::aes_string(x = "name", y = "val", fill = "type")
  else
    mp = ggplot2::aes_string(x = "name", y = "val")
  p = ggplot2::ggplot(data = data, mapping = mp)
  p = p + ggplot2::geom_bar(position = "identity", stat = "identity")
  p = p + ggplot2::labs(title = sprintf("%s (%i features), filter = %s",
                                        fvalues$task.desc$id,
                                        sum(fvalues$task.desc$n.feat), fvalues$method),
                        x = "", y = "")
  p = p + ggplot2::theme(axis.text.x = element_text(angle = 45, hjust = 1))
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
#' @param feat.type.cols [\code{logical(1)}]\cr
#'   Color factor and numeric features.
#'   \code{FALSE} means no colors.
#'   Default is \code{FALSE}.
#' @template ret_ggv
#' @export
#' @examples
#' fv = getFilterValues(iris.task, method = "chi.squared")
#' plotFilterValuesGGVIS(fv)
plotFilterValuesGGVIS = function(fvalues, sort = "dec", n.show = 20L, feat.type.cols = FALSE) {
  assertClass(fvalues, classes = "FilterValues")
  assertChoice(sort, choices = c("dec", "inc", "none"))
  assertFlag(feat.type.cols)
  n.show = asCount(n.show)

  data = fvalues$data
  n.show = min(n.show, sum(!is.na(data$val)))
  if (sort != "none")
    data = head(sortByCol(data, "val", asc = (sort == "inc")), n.show)

  data$name = factor(data$name, levels = as.character(data$name))
  if (feat.type.cols)
    p = ggvis::ggvis(data, ggvis::prop("x", as.name("name")),
                     ggvis::prop("y", as.name("val")),
                     ggvis::prop("fill", as.name("type")))
  else
    p = ggvis::ggvis(data, ggvis::prop("x", as.name("name")),
                     ggvis::prop("y", as.name("val")))

  p = ggvis::layer_bars(p)

  add_title <- function(vis, ..., x_lab = "", title = "") {
      vis = ggvis::add_axis(vis, "x", title = x_lab)
      vis = ggvis::add_axis(vis, "x", orient = "top", ticks = 0, title = title,
                     properties = ggvis::axis_props(
                         axis = list(stroke = "white"),
                         labels = list(fontSize = 0)
                     ), ...)
  }
  p = add_title(p, x_lab = "", title = sprintf("%s (%i features), filter = %s",
    fvalues$task.desc$id, sum(fvalues$task.desc$n.feat), fvalues$method))
  p = ggvis::add_axis(p, "y", title = "")
  return(p)
}
