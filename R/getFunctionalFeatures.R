# Get only functional features from a task.
getFunctionalFeatures = function(object, subset = NULL, features, recode.target = "no") {
  UseMethod("getFunctionalFeatures")
}

getFunctionalFeatures.Task = function(object, subset = NULL, features, recode.target = "no") {
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



