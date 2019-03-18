# Convert fd.features list to list of column indices and check for consitency.
fdFeatsToColumnIndex = function(df, fd.features = NULL, exclude.cols = NULL) {

  # If the data.frame already contains matricies, keep them
  fd.mats = which(vcapply(df, function(x) class(x)[1L]) == "matrix")

  # If fd.features is NULL, all numerics are a single functional feature
  # Already existing matricies are not converted
  if (is.null(fd.features))
    fd.features = list("fd1" = setdiff(which(vlapply(df, is.numeric)), c(exclude.cols, fd.mats)))

  # Return the column index and check if indices/names refer to columns
  lapply(fd.features, function(fd.feature) {
    if (is.character(fd.feature)) {
      assertSubset(fd.feature, colnames(df), empty.ok = FALSE)
      setdiff(which(colnames(df) %in% fd.feature), exclude.cols)
    } else {
      assertSubset(fd.feature, seq_len(ncol(df)))
      setdiff(fd.feature, exclude.cols)
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



