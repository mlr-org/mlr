#' Create a functional feature from a numeric matrix
#'
#' @param df [\code{data.frame}] \cr
#' @param fd.features [\code{list}] \cr
#'   Named list containing \code{Integer} column indices or \code{character} column names which
#'   indicate functional features.
#' @return [\code{data.frame}] \cr containing all features in a functional form, i.e. functional
#'   features as a matrix.
#' @export
makeFunctionalData = function(df, fd.features) {
  assertDataFrame(df)
  # Convert fd.features to column indices
  fd.features = fdFeatsToColumnIndex(df, fd.features)
  # Create functional features
  ffeats = lapply(fd.features, function(x) {makeFunctionalFeature(df[, x, drop = FALSE])})
  d = df[, - unlist(fd.features), drop = FALSE]
  d[, names(fd.features)] = ffeats
  return(d)
}

#' Create a functional feature from a numeric matrix
#'
#' @param m [\code{matrix}] \cr Numeric matrix.
#' @return [\code{matrix}] \cr Matrix of class c("functional", "matrix") containing the functional values
#' @export
makeFunctionalFeature = function(m) {
  if (is.data.frame(m))
    m = as.matrix(m)
  assertMatrix(m, mode = "numeric")
  addClasses(m, "functional")
}

# Convert fd.features to column indices
fdFeatsToColumnIndex = function(df, fd.features) {
  assertList(fd.features)
  sapply(fd.features, function(fdfeature) {
    if (is.character(fdfeature)) {
      assertSubset(fdfeature, colnames(df))
      fd.features = which(colnames(df) %in% fdfeature)
    } else {
      assertSubset(fdfeature, seq_len(nrow(df)))
    }
  }, USE.NAMES = TRUE)
}

# Convert a data.frame containing functional features to a data.frame containing
# them as numerics.
functionalToNormalData = function(df) {
  if (!hasFunctionalFeatures(df)) {
    df
  } else {
    do.call(data.frame(as.list(d)))
  }
}

# check if the data.frame has functional features
hasFunctionalFeatures = function(df) {
  if (any(sapply(df, class)) %in% "matrix") {
    TRUE
  } else {
    FALSE
  }
}
