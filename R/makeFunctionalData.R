#' @title Create a data.frame containing functional features from a normal data.frame.
#'
#' @description
#' To work with functional features, they need to be
#' stored as a \code{matrix} colmun in the data.frame so \code{mlr} can automatically
#' recognize them as functional features.
#' This function allows an easy conversion from a normal data.frame to the required format.
#'
#' @param data [\code{data.frame}] \cr
#'   A data.frame that contains the functional features as numeric columns.
#' @param fd.features [\code{list}] \cr
#'   Named list containing \code{integer} column indices or \code{character} column names.
#'   Each element defines a functional feature, in the given order of the indices or column names.
#'   The name of the list element defines the name of the functional feature.
#'   All selected columns have to correspond to numeric data.frame entries.
#'   If the list is empty, all numeric features are considered functional.
#' @param exclude.cols [\code{character} | \code{integer}]\cr
#'   Column names or indices to exclude from conversion to functionals, even if they
#'   are in included in \code{fd.features}.
#'   Default is not to exclude anything.
#' @return [\code{data.frame}].
#' @export
#' @examples
#' # data.frame where columns 1:6 and 8:10 belong to a functional feature
#' d1 = data.frame(matrix(rnorm(100), nrow = 10), "target" = seq_len(10))
#' # Transform to functional data
#' d2 = makeFunctionalData(d1, fd.features = list("fd1" = 1:6, "fd2" = 8:10))
#' # Create a regression task
#' makeRegrTask(data = d2, target = "target")
makeFunctionalData = function(data, fd.features = NULL, exclude.cols = integer(0L)) {
  assertDataFrame(data)
  assertList(fd.features)

  # Convert fd.features to column indices
  fd.features = fdFeatsToColumnIndex(data, fd.features, exclude.cols)
  # Create a list of functional feature matricies
  ffeats = lapply(fd.features, function(x) {makeFunctionalFeature(data[, x, drop = FALSE])})
  # Drop original numeric data
  d = data[, - unlist(fd.features), drop = FALSE]
  # Add functional feature matricies
  d[, names(fd.features)] = ffeats
  return(d)
}

