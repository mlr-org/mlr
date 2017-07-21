# Convert fd.features list to list of column indices
fdFeatsToColumnIndex = function(df, fd.features = NULL, exclude.cols = NULL) {

  # If the data.frame already contains matricies, keep them
  if (hasFunctionalFeatures(df)) {
    ids = which(vcapply(df, function(x) class(x)[1L]) == "matrix")
    exclude.cols = c(exclude.cols, ids)
  }

  # If a target column is provided we exclude it from any functional feature
  # Target can either be a colum name, column index or NULL.
  if (class(exclude.cols) == "character") {
    exclude.cols = which(colnames(df) %in% exclude.cols)
  } else {
    assertSubset(exclude.cols, c(seq_len(nrow(df)), NULL))
  }

  # If fd.features is NULL, all numerics are a functional feature
  if (is.null(fd.features))
    fd.features = list("fd1" = setdiff(which(vlapply(df, is.numeric)), exclude.cols))

  lapply(fd.features, function(fdfeature) {
    if (is.character(fdfeature)) {
      assertSubset(fdfeature, colnames(df), empty.ok = FALSE)
      setdiff(which(colnames(df) %in% fdfeature), exclude.cols)
    } else {
      assertSubset(fdfeature, seq_len(ncol(df)))
      setdiff(fdfeature, exclude.cols)
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



