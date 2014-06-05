#' @title Calculates feature importance values.
#'
#' @description
#' Calculates numerical importance values for all features.
#' Look at package \code{\link[FSelector]{FSelector}} for details on the filter algorithms.
#'
#' Available \code{method}s are:
#' \tabular{lll}{
#'   linear.correlation         \tab R      \tab
#'     Pearson's correlation between feature and target \cr
#'   rank.correlation           \tab R      \tab
#'     Spearman's correlation between feature and target \cr
#'   information.gain           \tab C,R    \tab
#'     Entropy-based information gain between feature and target \cr
#'   gain.ratio                 \tab C,R    \tab
#'     Entropy-based gain ratio between feature and target \cr
#'  symmetrical.uncertainty     \tab C,R    \tab
#'     Entropy-based symmetrical uncertainty between feature and target \cr
#'   chi.squared                \tab C,R    \tab
#'     Chi-squared statistic of independence between feature and target \cr
#'   random.forest.importance   \tab C,R    \tab
#'     See \code{\link[randomForest]{importance}} \cr
#'   relief                     \tab C,R    \tab
#'     RELIEF algorithm \cr
#'   oneR                       \tab C,R    \tab
#'     \code{\link[RWeka]{OneR}} assocation rule \cr
#' }
#'
#' @template arg_taskdf
#' @template arg_taskdf_target
#' @param method [\code{character(1)}]\cr
#'   Filter method, see above.
#'   Default is \dQuote{random.forest.importance}.
#' @param ... [any]\cr
#'   Passed down to selected method.
#' @return [named \code{numeric}]. Importance value for each feature.
#' @export
#' @family filter
getFilterValues = function(obj, target, method = "random.forest.importance", ...) {
  checkArg(obj, c("data.frame", "SupervisedTask"))
  checkArg(method, choices = getFilterMethods())
  requirePackages("FSelector", why = "getFilterValues")
  UseMethod("getFilterValues")
}

#' @export
getFilterValues.SupervisedTask = function(obj, target, method = "random.forest.importance", ...) {
  if (method %in% c("linear.correlation", "rank.correlation")) {
    if (!inherits(obj, "RegrTask") || (obj$task.desc$n.feat["factors"] > 0L))
      stop("Method can only be applied for a regression task with numerical data!")
  }
  getFilterValues(getTaskData(obj), obj$task.desc$target, method)
}

#' @export
getFilterValues.data.frame = function(obj, target, method = "random.forest.importance", ...) {
  checkArg(target, "character")
  fun = get(method, envir = getNamespace("FSelector"))
  f = paste(target, "~.")
  y = fun(as.formula(f), obj, ...)
  vals = y[,1L]
  names(vals) = rownames(y)
  vals
}
