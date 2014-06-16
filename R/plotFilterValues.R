#' Plot filter values.
#'
#' @param fvalues [\code{\link{FilterValues}}]\cr
#'   Filtr values.
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
  checkArg(fvalues, "FilterValues")
  checkArg(sort, choices = c("dec", "inc", "none"))
  n.show = convertInteger(n.show)
  checkArg(n.show, "integer", min.len = 1L, na.ok = FALSE)

  d = fvalues$data
  k = nrow(d)
  n.show = min(n.show, k)
  if (sort != "none")
    d = sortByCol(d, "val", asc = (sort == "inc"))
  # resort for ggplot
  d$name = factor(d$name, levels = d$name)
  d = d[1:n.show,, drop = FALSE]
  if (!is.null(feat.type.cols)) {
    checkArg(feat.type.cols, "character", len = 2L, na.ok = FALSE)
    mp = aes_string(x = "name", y = "val", fill = "type")
  } else {
    mp = aes_string(x = "name", y = "val")
  }
  p = ggplot(data = d, mapping = mp)
  p = p + geom_bar(stat = "identity")
  if (!is.null(feat.type.cols))
    p = p + scale_fill_manual(values = feat.type.cols)
  p = p + ggtitle(sprintf("%s (%i features), filter = %s",
    fvalues$task.desc$id, k , fvalues$method))
  p = p + xlab("") + ylab("")
  p = p + theme(axis.text.x = element_text(angle = 45, hjust = 1))
  return(p)
}
