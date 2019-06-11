#' @title Create a data.frame containing functional features from a normal data.frame.
#'
#' @description
#' To work with functional features, those features need to be
#' stored as a `matrix` column in the data.frame, so `mlr` can automatically
#' recognize them as functional features.
#' This function allows for an easy conversion from a data.frame with numeric columns
#' to the required format. If the data already contains matrix columns, they are left as-is
#' if not specified otherwise in `fd.features`. See `Examples` for the structure
#' of the generated output.
#'
#' @param data ([data.frame]) \cr
#'   A data.frame that contains the functional features as numeric columns.
#' @param fd.features ([list]) \cr
#'   Named list containing `integer` column indices or `character` column names.
#'   Each element defines a functional feature, in the given order of the indices or column names.
#'   The name of the list element defines the name of the functional feature.
#'   All selected columns have to correspond to numeric data.frame entries.
#'   The default is `NULL`, which means all numeric features are considered
#'   to be a single functional \dQuote{fd1}.
#' @param exclude.cols ([character] | [integer])\cr
#'   Column names or indices to exclude from conversion to functionals, even if they
#'   are in included in `fd.features`.
#'   Default is not to exclude anything.
#' @return ([data.frame]).
#' @export
#' @examples
#' # data.frame where columns 1:6 and 8:10 belong to a functional feature
#' d1 = data.frame(matrix(rnorm(100), nrow = 10), "target" = seq_len(10))
#' # Transform to functional data
#' d2 = makeFunctionalData(d1, fd.features = list("fd1" = 1:6, "fd2" = 8:10))
#' # Create a regression task
#' makeRegrTask(data = d2, target = "target")
makeFunctionalData = function(data, fd.features = NULL, exclude.cols = NULL) {

  assertDataFrame(data)
  assertList(fd.features, null.ok = TRUE, names = "unique")
  # Assert that exclude.cols refers to valid columns and convert to index
  if (is.character(exclude.cols)) {
    assertSubset(exclude.cols, colnames(data))
    exclude.cols = which(colnames(data) %in% exclude.cols)
  } else {
    assertSubset(exclude.cols, seq_len(ncol(data)))
  }

  # If fd.features is an empty list do nothing
  if (is.list(fd.features) && length(fd.features) == 0L) {
    return(data)
  }

  # Convert fd.features to column indices
  fd.features = fdFeatsToColumnIndex(data, fd.features, exclude.cols)

  # All fd.features must refer to numeric columns
  if (!all(vlapply(data[, unlist(fd.features), drop = FALSE], is.numeric))) {
    stop("fd.features contains non-integer/numeric columns")
  }

  # Create a list of functional feature matricies
  ffeats = lapply(fd.features, function(x) {
    as.matrix(data[, x, drop = FALSE])
  })
  # Drop original numeric data
  d = data[, -unlist(fd.features), drop = FALSE]
  # Add functional feature matricies
  d[, names(fd.features)] = ffeats
  return(d)
}
