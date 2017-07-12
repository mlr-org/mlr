#' Create a functional feature from a numeric matrix
#'
#' @param df [\code{data.frame}] \cr
#' @param fd.features [\code{list}] \cr
#'   Named list containing \code{Integer} column indices or \code{character} column names which
#'   indicate functional features.
#'   If the list is empty, all numeric features are considered functional.
#' @return [\code{data.frame}] \cr containing all features in a functional form, i.e. functional
#'   features as a matrix.
#' @export
makeFunctionalData = function(df, fd.features = list()) {
  assertDataFrame(df)
  assertList(fd.features)
  # Convert fd.features to column indices
  fd.features = fdFeatsToColumnIndex(df, fd.features)
  # Create a list of functional feature matricies
  ffeats = lapply(fd.features, function(x) {makeFunctionalFeature(df[, x, drop = FALSE])})
  # Drop original numeric data
  d = df[, - unlist(fd.features), drop = FALSE]
  # Add functional feature matricies
  d[, names(fd.features)] = ffeats
  return(d)
}

