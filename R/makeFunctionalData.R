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
#'   The default is \code{NuLL}, which means all numeric features are considered
#'   to be a single functional \dQuote{fd1}.
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
  assertList(fd.features, null.ok = TRUE)

  # Convert fd.features to column indices
  fd.features = fdFeatsToColumnIndex(data, fd.features, exclude.cols)

  # All fd.features must refer to numeric or integer columns
  stopifnot(unique(vcapply(data[, unlist(fd.features), drop = FALSE], class)) %in%
      c("numeric", "integers"))
  # If an empty list is provided, return the original data.
  if (length(fd.features) == 0L)
    return(data)

  # Create a list of functional feature matricies
  ffeats = lapply(fd.features, function(x) {as.matrix(data[, x, drop = FALSE])})
  # Drop original numeric data
  d = data[, - unlist(fd.features), drop = FALSE]
  # Add functional feature matricies
  d[, names(fd.features)] = ffeats
  return(d)
}

