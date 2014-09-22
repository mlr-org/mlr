#' Plot filter values.
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
  requirePackages("ggplot2", why = "getFilterValues")
  n.show = asCount(n.show)

  data = fvalues$data
  n.show = min(n.show, sum(!is.na(scores)))
  if (sort != "none")
    data = head(sortByCol(data, "val", asc = (sort == "inc")), n.show)

  data$name = factor(data$name, levels = as.character(data$name))
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
  p = p + ggtitle(sprintf("%s (%i features), filter = %s",
    fvalues$task.desc$id, sum(fvalues$task.desc$n.feat), fvalues$method))
  p = p + xlab("") + ylab("")
  p = p + theme(axis.text.x = element_text(angle = 45, hjust = 1))
  return(p)
}
