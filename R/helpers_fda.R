# Convert fd.features list to list of column indices
fdFeatsToColumnIndex = function(df, fd.features = NULL, exclude.cols = NULL) {

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

#' Check whether the object has functional features.
#'
#' @param obj [\code{Task|TaskDesc|data.frame}]\cr
#'   Object to check.
#' @return [\code{logical(1)}]
#' @export
hasFunctionalFeatures = function(obj) {
  UseMethod("hasFunctionalFeatures")
}

hasFunctionalFeatures.data.frame = function(obj) {
  # Check if the data.frame contains matricies
  ifelse(any(vcapply(obj, function(x) class(x)[1L]) == "matrix"), TRUE, FALSE)
}

hasFunctionalFeatures.Task = function(obj) {
  # Pass on the task.desc
  hasFunctionalFeatures.TaskDesc(obj$task.desc)
}

hasFunctionalFeatures.TaskDesc = function(obj) {
  # Check if the task.desc has functionals
  obj$n.feat["functionals"] > 0L
}


# Get only functional features from a task.
getFunctionalFeatures = function(object, subset = NULL, features, recode.target = "no"){
  UseMethod("getFunctionalFeatures")
}

getFunctionalFeatures.Task = function(object, subset = NULL, features, recode.target = "no"){
  # Get data and pass on to data.frame method
  df = getTaskData(object, subset, features, target.extra = TRUE, recode.target, functionals.as = "matrix")
  getFunctionalFeatures.data.frame(df$data)
}

getFunctionalFeatures.data.frame = function(object, subset = NULL, features, recode.target = "no"){
  # Keep only columns with class matrix
  funct.cols = which(vcapply(object, function(x) class(x)[1L]) == "matrix")
  if (length(funct.cols) == 0)
    stop("No functional features in the data")
  object[, funct.cols, drop = FALSE]
}


