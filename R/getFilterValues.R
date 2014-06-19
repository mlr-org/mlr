#' @title Calculates feature filter values.
#'
#' @description
#' Calculates numerical filter values for all features.
#' Look at package \code{\link[FSelector]{FSelector}} for details on the filter algorithms.
#'
#' Currently only supports classification (C) and regression (R). Allowed feature types are abbreviated
#' in table as numerics (N) and factors (F).
#'
#' Available \code{method}s are:
#' \tabular{llll}{
#'   Method                     \tab Tasks \tab Feats \tab Description \cr
#'   linear.correlation         \tab R     \tab N     \tab
#'     Pearson's correlation between feature and target \cr
#'   rank.correlation           \tab R     \tab N     \tab
#'     Spearman's correlation between feature and target \cr
#'   information.gain           \tab C,R   \tab N,F   \tab
#'     Entropy-based information gain between feature and target \cr
#'   gain.ratio                 \tab C,R   \tab N,F   \tab
#'     Entropy-based gain ratio between feature and target \cr
#'   symmetrical.uncertainty    \tab C,R   \tab N,F   \tab
#'     Entropy-based symmetrical uncertainty between feature and target \cr
#'   chi.squared                \tab C,R   \tab N,F   \tab
#'     Chi-squared statistic of independence between feature and target \cr
#'   random.forest.importance   \tab C,R   \tab N,F   \tab
#'     See \code{\link[randomForest]{importance}} \cr
#'   relief                     \tab C,R   \tab N,F   \tab
#'     RELIEF algorithm \cr
#'   oneR                       \tab C,R   \tab N,F   \tab
#'     \code{\link[RWeka]{OneR}} assocation rule \cr
#'   mRMR.classic               \tab R     \tab N     \tab
#'     Classic mRMR algorithm from \code{\link[mRMRe]{mRMRe}} package \cr
#' }
#'
#' @template arg_task
#' @param method [\code{character(1)}]\cr
#'   Filter method, see above.
#'   Default is \dQuote{random.forest.importance}.
#' @param ... [any]\cr
#'   Passed down to selected method.
#' @return [\code{\link{FilterValues}}].
#' @family filter
#' @export
getFilterValues = function(task, method = "random.forest.importance", ...) {
  checkArg(task, c("ClassifTask", "RegrTask"))
  checkArg(method, choices = getFilterMethods())
  
  if (method %in% c("linear.correlation", "rank.correlation", "mRMR.classic")) {
    if (!inherits(task, "RegrTask") || (task$task.desc$n.feat["factors"] > 0L))
      stopf("Method '%s' can only be applied for a regression task with numerical data!", method)
  }

  data = getTaskData(task)
  
  if (method %in% c("mRMR.classic")) {
    requirePackages("mRMRe", why = "getFilterValues")
    fun = get(method, envir = getNamespace("mRMRe"))
    
    target.ind = which(getTargetNames(task) == colnames(data))
    data.mrmr = mRMR.data(data = data)
    res = fun(data = data.mrmr, target_indices = target.ind, 
              feature_count = length(data.mrmr@feature_names) - 1)
    
    y = as.vector(scores(res)[[1]])
    ns = res@feature_names[as.vector(solutions(res)[[1]])]
    
  } else {
    requirePackages("FSelector", why = "getFilterValues")
    fun = get(method, envir = getNamespace("FSelector"))
    f = getTaskFormulaAsString(task)
    
    y = fun(as.formula(f), data, ...)
    ns = rownames(y)
    y = y[,1L]
  }
    
  types = sapply(data[, ns, drop = FALSE], getClass1)
  makeS3Obj("FilterValues",
    task.desc = task$task.desc,
    method = method,
    data = data.frame(
      name = ns,
      val = y,
      type = types,
      stringsAsFactors = FALSE
    )
  )
}

#' Result of \code{\link{getFilterValues}}.
#'
#' \itemize{
#'   \item{task.desc [\code{\link{TaskDesc}}]}{Task description.}
#'   \item{method [\code{character}]}{Filter method.}
#'   \item{data [\code{data.frame}]}{Has columns: \code{name} = Names of features;
#'     \code{val} = Feature importance values; \code{type} = Feature column type.}
#' }
#' @name FilterValues
#' @rdname FilterValues
#' @family filter
NULL

#' @export
print.FilterValues = function(x, ...) {
  catf("FilterValues:")
  catf("Task: %s", x$task.desc$id)
  catf("Method: %s", x$method)
  print(head(as.data.frame(x$data)))
}

