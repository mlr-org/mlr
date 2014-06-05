#' @title Calculates feature importance values.
#'
#' @description
#' Calculates numerical importance values for all features.
#' Look at package \code{\link[FSelector]{FSelector}} for details on the filter algorithms.
#'
#' Currently only supports classification and regression.
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
#' @template arg_task
#' @param method [\code{character(1)}]\cr
#'   Filter method, see above.
#'   Default is \dQuote{random.forest.importance}.
#' @param ... [any]\cr
#'   Passed down to selected method.
#' @return [named \code{numeric}]. Importance value for each feature.
#' @export
#' @family filter
getFilterValues = function(task, method = "random.forest.importance", ...) {
  checkArg(task, c("ClassifTask", "RegrTask"))
  checkArg(method, choices = getFilterMethods())
  requirePackages("FSelector", why = "getFilterValues")

  if (method %in% c("linear.correlation", "rank.correlation")) {
    if (!inherits(task, "RegrTask") || (task$task.desc$n.feat["factors"] > 0L))
      stop("Method can only be applied for a regression task with numerical data!")
  }

  fun = get(method, envir = getNamespace("FSelector"))
  f = getTaskFormulaAsString(task)
  y = fun(as.formula(f), getTaskData(task), ...)
  setNames(y[,1L], rownames(y))
}

