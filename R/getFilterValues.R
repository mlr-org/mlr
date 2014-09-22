#' @title Calculates feature filter values.
#'
#' @description
#' Calculates numerical filter values for features.
#' For a list of features, use \code{\link{listFilterMethods}}.
#'
#' @template arg_task
#' @param method [\code{character(1)}]\cr
#'   Filter method, see above.
#'   Default is \dQuote{rf.importance}.
#' @param nselect [\code{integer(1)}]\cr
#'   Number of scores to request. Scores are getting calculated for all features per default.
#' @param ... [any]\cr
#'   Passed down to selected method.
#' @return [\code{\link{FilterValues}}].
#' @family filter
#' @export
getFilterValues = function(task, method = "rf.importance", nselect = getTaskNFeats(task), ...) {
  assert(checkClass(task, "ClassifTask"), checkClass(task, "RegrTask"), checkClass(task, "SurvTask"))
  filters = getFilterRegister()
  assertChoice(method, choices = ls(filters))
  td = task$task.desc
  filter = filters[[method]]

  if (!isScalarNA(filter$pkg))
    requirePackages(filter$pkg, why = "getFilterValues")
  if (td$type %nin% filter$supported.tasks)
    stopf("Filter '%s' not compatible with task of type '%s'", filter$name, td$type)
  unsupported = setdiff(names(td$n.feat[td$n.feat > 0L]), filter$supported.features)
  if (length(unsupported) > 0L)
    stopf("Filter '%s' does not support features of type '%s'", filter$name, unsupported[1L])
  assertCount(nselect)

  res = do.call(filter$fun, list(task = task, nselect = nselect))

  fn = getTaskFeatureNames(task)
  missing.score = setdiff(fn, names(res))
  res[missing.score] = NA_real_
  res = res[match(names(res), fn)]
  makeS3Obj("FilterValues",
    task.desc = td,
    method = method,
    data = data.frame(
      name = names(res),
      val = unname(res),
      type = vcapply(getTaskData(task, target.extra = TRUE)$data[fn], getClass1),
      row.names = NULL,
      stringsAsFactors = FALSE)
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
  print(head(x$data))
}
