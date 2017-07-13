#' Create a functional feature from a numeric matrix
#'
#' @param mat [\code{matrix|data.frame}] \cr
#'  Numeric matrix or data.frame that contains the functional features.
#' @return [\code{matrix}] \cr
#'   Matrix of class c("functional", "matrix") containing the functional values
#' @export
makeFunctionalFeature = function(mat) {
  if (is.data.frame(mat))
    mat = as.matrix(mat)
  assertMatrix(mat, mode = "numeric")
  addClasses(mat, "functional")
}


# Convert fd.features list to list of column indices
fdFeatsToColumnIndex = function(df, fd.features = list(), target = NULL) {
  assertList(fd.features)

  # If the data.frame already contains matricies, keep them
  if (hasFunctionalFeatures(df)) {
    ids = which(sapply(df, class) == "matrix")
    fd.features = c(fd.features, ids)
  }

  # If fd.features is an empty list, all numerics are a functional feature
  if (length(fd.features) == 0)
    fd.features = list("fd1" = setdiff(which(sapply(df, is.numeric)), which(colnames(df) == target)))

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
  if (hasFunctionalFeatures(df)) {
    df = do.call(data.frame, as.list(df))
    message("Functional features have been converted to numerics")
  }
  return(df)
}

# Check if the data.frame has functional features
hasFunctionalFeatures = function(df) {
  bool = FALSE
  if (any(unlist(sapply(df, class)) %in% "matrix"))
    bool = TRUE
  return(bool)
}
