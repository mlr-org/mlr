#' @title Calculates feature filter values.
#'
#' @description
#' Calculates numerical filter values for all features.
#' Look at package \code{\link[FSelector]{FSelector}} for details on the filter algorithms.
#'
#' Currently only supports classification (C), regression (R) and survival (S) tasks. Allowed
#' feature types are abbreviated in table as numerics (N) and factors (F).
#'
#' Available \code{method}s are:
#' \tabular{llll}{
#'   Method                      \tab Tasks \tab Feats \tab Description \cr
#'   linear.correlation           \tab R     \tab N     \tab
#'     Pearson's correlation between feature and target \cr
#'   rank.correlation             \tab R     \tab N     \tab
#'     Spearman's correlation between feature and target \cr
#'   information.gain             \tab C,R   \tab N,F   \tab
#'     Entropy-based information gain between feature and target \cr
#'   gain.ratio                   \tab C,R   \tab N,F   \tab
#'     Entropy-based gain ratio between feature and target \cr
#'   symmetrical.uncertainty      \tab C,R   \tab N,F   \tab
#'     Entropy-based symmetrical uncertainty between feature and target \cr
#'   chi.squared                  \tab C,R   \tab N,F   \tab
#'     Chi-squared statistic of independence between feature and target \cr
#'   random.forest.importance     \tab C,R   \tab N,F   \tab
#'     See \code{\link[randomForest]{importance}} \cr
#'   randomForstSRC.importance    \tab C,R,S \tab N,F   \tab
#'     See \code{\link[randomForestSRC]{vimp}} \cr
#'   randomForstSRC.minimal.depth \tab C,R,S \tab N,F   \tab
#'     See \code{\link[randomForestSRC]{var.select}} \cr
#'   relief                       \tab C,R   \tab N,F   \tab
#'     RELIEF algorithm \cr
#'   oneR                         \tab C,R   \tab N,F   \tab
#'     \code{\link[RWeka]{OneR}} assocation rule \cr
#'   mRMR.classic                 \tab R     \tab N     \tab
#'     MRMR algorithm, see \code{\link[mRMRe]{mRMR.classic}} from \code{mRMRe} package \cr
#'   carscore                     \tab R     \tab N     \tab
#'     CAR score, see \code{\link[care]{carscore}} from \code{care} package \cr
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
  assert(checkClass(task, "ClassifTask"), checkClass(task, "RegrTask"), checkClass(task, "SurvTask"))
  assertChoice(method, choices = listFilterMethods())

  if (method %in% c("linear.correlation", "rank.correlation", "mRMR.classic", "carscore")) {
    if (!inherits(task, "RegrTask") || (task$task.desc$n.feat["factors"] > 0L))
      stopf("Method '%s' can only be applied for a regression task with numerical data!", method)
  }
  if (!grepl('randomForestSRC', method) & inherits(task, "SurvTask")) {
     stopf("Method '%s' can not be applied for a survival task!", method)
  }

  data = getTaskData(task)

  if (method == "mRMR.classic") {
    requirePackages("mRMRe", why = "getFilterValues")
    fun = get(method, envir = getNamespace("mRMRe"))

    target.ind = which(getTargetNames(task) == colnames(data))
    data.mrmr = mRMR.data(data = data)
    res = fun(data = data.mrmr, target_indices = target.ind,
      feature_count = length(data.mrmr@feature_names) - 1)

    y = as.vector(scores(res)[[1]])
    ns = res@feature_names[as.vector(solutions(res)[[1]])]
  } else if (method == "carscore") {
    requirePackages("care", why = "getFilterValues")
    target.ind = which(getTargetNames(task) == colnames(data))
    y = carscore(Xtrain = data[, -target.ind], Ytrain = data[, target.ind], verbose = FALSE)
    ns = names(y)
    y = as.vector(y)
  } else if (method == "randomForestSRC.importance"){
    requirePackages("randomForestSRC", why = "getFilterValues")
    im = rfsrc(getTaskFormula(task), data = data, proximity = FALSE, forest = FALSE, ...)$importance
    if (inherits(task, "ClassifTask")) {
      ns = rownames(im)
      y = im[,"all"] 
    } else {
      ns = names(im)
      y = unname(im)
    }
  } else if (method == "randomForestSRC.minimal.depth"){
    requirePackages("randomForestSRC", why = "getFilterValues")
    im = var.select(getTaskFormula(task), data, method = "md", verbose = FALSE)$md.obj$order
    ns = rownames(im)
    y = -im[,1]
  } else {
    requirePackages("FSelector", why = "getFilterValues")
    fun = get(method, envir = getNamespace("FSelector"))
    f = getTaskFormulaAsString(task)

    y = fun(as.formula(f), data, ...)
    ns = rownames(y)
    y = y[, 1L]
  }

  types = vcapply(data[, ns, drop = FALSE], getClass1)
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

