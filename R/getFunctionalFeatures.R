# @title Get only functional features from a task or a data.frame.
#
# @description
# The parameters \dQuote{subset}, \dQuote{features}, and \dQuote{recode.target}
# are ignored for the data.frame method.
# @inheritParams getTaskData
# @return Returns a [\code{data.frame}] containing only the functional features.
# @export
getFunctionalFeatures = function(object, subset = NULL, features, recode.target = "no") {
  UseMethod("getFunctionalFeatures")
}

# @export
# @rdname getFunctionalFeatures
getFunctionalFeatures.Task = function(object, subset = NULL, features, recode.target = "no") {
  # Get data and pass on to data.frame method
  df = getTaskData(object, subset, features, target.extra = TRUE, recode.target, functionals.as = "matrix")
  getFunctionalFeatures.data.frame(df$data)
}

# @export
# @rdname getFunctionalFeatures
getFunctionalFeatures.data.frame = function(object, subset = NULL, features, recode.target = "no") {
  # Keep only columns with class matrix
  funct.cols = which(vcapply(object, function(x) class(x)[1L]) == "matrix")
  if (length(funct.cols) == 0L) {
    stop("No functional features in the data")
  }
  object[, funct.cols, drop = FALSE]
}
