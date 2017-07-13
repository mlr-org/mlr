#' Create a data.frame containing functional features from a normal data.frame
#'
#' @param df [\code{data.frame}] \cr
#' @param fd.features [\code{list}] \cr
#'   Named list containing \code{integer} column indices or \code{character} column names which
#'   indicate functional features.
#'   If the list is empty, all numeric features are considered functional.
#' @return [\code{data.frame}] \cr Contains all features specified in \code{fd.features}
#'   in a functional form, i.e. functional features as a matrix.
#' @export
#' @examples
#' # data.frame where columns 1:6 and 8:10 belong to a functional feature
#' df = data.frame(matrix(rnorm(100), nrow = 10), "target" = seq_len(10))
#' # Transform to functional data
#' fdf = makeFunctionalData(df, fd.features = list("fd1" = 1:6, "fd2" = 8:10))
#' # Create a regression task
#' makeRegrTask(data = fdf, target = "target")
makeFunctionalData = function(df, fd.features = list(), target = NULL) {
  assertDataFrame(df)
  assertList(fd.features)

  # Convert fd.features to column indices
  fd.features = fdFeatsToColumnIndex(df, fd.features, target)
  # Create a list of functional feature matricies
  ffeats = lapply(fd.features, function(x) {makeFunctionalFeature(df[, x, drop = FALSE])})
  # Drop original numeric data
  d = df[, - unlist(fd.features), drop = FALSE]
  # Add functional feature matricies
  d[, names(fd.features)] = ffeats
  return(d)
}

