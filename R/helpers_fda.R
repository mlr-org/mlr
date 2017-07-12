#' Create a functional feature from a numeric matrix
#'
#' @param m [\code{matrix|data.frame}] \cr
#'  Numeric matrix or data.frame that contains the functional features.
#' @return [\code{matrix}] \cr
#'   Matrix of class c("functional", "matrix") containing the functional values
#' @export
makeFunctionalFeature = function(m) {
  if (is.data.frame(m))
    m = as.matrix(m)
  assertMatrix(m, mode = "numeric")
  addClasses(m, "functional")
}

# Convert fd.features list to list of column indices
fdFeatsToColumnIndex = function(df, fd.features = list()) {
  assertList(fd.features)

  # If the data.frame already contains matricies, keep them
  if (hasFunctionalFeatures(df)) {
    ids = which(sapply(df, class) == "matrix")
    fd.features[names(ids)] = ids
  }

  # If fd.features is an empty list, all numerics are a functional feature
  if (length(fd.features) == 0)
    fd.features = list("fd1" = which(sapply(df, is.numeric)))

  lapply(fd.features, function(fdfeature) {
    if (is.character(fdfeature)) {
      assertSubset(fdfeature, colnames(df))
      fd.features = which(colnames(df) %in% fdfeature)
    } else {
      assertSubset(fdfeature, seq_len(ncol(df)))
    }
  })
}

# Convert a data.frame containing functional features to a data.frame containing
# them as numerics.
functionalToNormalData = function(df) {
  if (hasFunctionalFeatures(df))
    df = do.call(data.frame(as.list(d)))
  return(df)
}

# Check if the data.frame has functional features
hasFunctionalFeatures = function(df) {
  bool = FALSE
  if (any(sapply(df, class) %in% "matrix"))
    bool = TRUE
  return(bool)
}
