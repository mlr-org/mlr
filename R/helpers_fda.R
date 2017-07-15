#' Create a functional feature from a numeric matrix
#'
#' @param mat [\code{matrix|data.frame}] \cr
#'  Numeric matrix or data.frame that contains the functional features.
#' @return [\code{matrix}] \cr
#'   Matrix of class "matrix" containing the functional values
#' @export
makeFunctionalFeature = function(mat) {
  if (is.data.frame(mat))
    mat = as.matrix(mat)
  if (any(dim(mat) %in% 0))
    stop("Matrix dimensions need to be > 0.")
  assertMatrix(mat, mode = "numeric")
  return(mat)
}


# Convert fd.features list to list of column indices
fdFeatsToColumnIndex = function(df, fd.features = list(), exclude.cols = NULL) {
  assertList(fd.features)

  # If the data.frame already contains matricies, keep them
  if (hasFunctionalFeatures(df)) {
    ids = which(vcapply(df, function(x) class(x)[1L]) == "matrix")
    fd.features = c(fd.features, ids)
  }

  # If a target column is provided we exclude it from any functional feature
  # Target can either be a colum name, column index or NULL.
  if (class(exclude.cols) == "character") {
    exclude.cols = which(colnames(df) %in% exclude.cols)
  } else {
    assertSubset(exclude.cols, c(seq_len(nrow(df)), NULL))
  }

  # If fd.features is an empty list, all numerics are a functional feature
  if (length(fd.features) == 0)
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


hasFunctionalFeatures = function(obj) {
  UseMethod("hasFunctionalFeatures")
}

hasFunctionalFeatures.data.frame = function(obj) {
  # Check if the data.frame contains matricies
  ifelse(any(vcapply(obj, function(x) class[1L]) == "matrix"), TRUE, FALSE)
}

hasFunctionalFeatures.Task = function(obj) {
  # Pass on the task.desc
  hasFunctionalFeatures(obj$task.desc)
}

hasFunctionalFeatures.TaskDesc = function(obj) {
  # Check if the task.desc has functionals
  obj$n.feat["functionals"] > 0L
}


# FIXME: Not sure if this is needed
# Get only functional features from a task.
getFunctionalFeatures = function(object, subset = NULL, features, recode.target = "no"){
  UseMethod("getFunctionalFeatures")
}

getFunctionalFeatures.Task = function(object, subset = NULL, features, recode.target = "no"){
  # Get data and pass on to data.frame method
  df = getTaskData(object, subset, features, target.extra = TRUE, recode.target, functionals.as = "matrix")
  getFunctionalFeatures(df$data)
}

getFunctionalFeatures.data.frame = function(object, subset = NULL, features, recode.target = "no"){
  # Keep only columns with class matrix
  funct.cols = which(vcapply(object, function(x) class(x)[1L]) == "matrix")
  if (length(funct.cols) == 0)
    stop("No functional features in the data")
  object[, funct.cols, drop = FALSE]
}


