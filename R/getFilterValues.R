#' @title Calculates feature filter values.
#'
#' @description
#' Calculates numerical filter values for features.
#' For a list of features, use \code{\link{listFilterMethods}}.
#'
#' @template arg_task
#' @param method [\code{character}]\cr
#'   Filter methods, see above.
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
  assert(all(sapply(method, function(x) assertChoice(x, choices = ls(.FilterRegister)))))
  td = task$task.desc
  filter = lapply(method, function(x) .FilterRegister[[x]])
  if (!(any(sapply(filter, function(x) !isScalarNA(filter$pkg)))))
    dummy = lapply(filter, function(x)
      requirePackages(x$pkg, why = "getFilterValues", default.method = "load"))
  check_task = sapply(filter, function(x) td$type %nin% x$supported.tasks)
  if (any(check_task))
    stopf("Filter(s) '%s' not campatible with task of type '%s'",
          paste(method[check_task], collapse = ", "), td$type)

  check_feat = lapply(filter, function(x) setdiff(names(td$nfeat[td$n.feat > 0L]), x$supported.features))
  check_length = sapply(check_feat, length) > 0L
  if (any(check_length)) {
    unsupported = check_feat[check_length]
    stopf("Filter(s) '%s' not compatible with features of type '%s' respectively.",
          method[check_length],
          paste(sapply(check_feat[check_length], function(x) paste(x, collapse = ", ")), collapse = ", and"))
  }
  assertCount(nselect)

  fn = getTaskFeatureNames(task)
  res = lapply(filter, function(x) {
    x = do.call(x$fun, c(list(task = task, nselect = nselect), list(...)))
    missing.score = setdiff(fn, names(x))
    x[missing.score] = NA_real_
    x = x[match(names(x), fn)]
    data.frame(name = names(x),
               type = vcapply(getTaskData(task, target.extra = TRUE)$data[fn], getClass1),
               val = unname(x),
               row.names = NULL,
               stringsAsFactors = FALSE)
  })
  names(res) = method
  makeS3Obj("FilterValues",
    task.desc = td,
    data = res
  )
}
#' Result of \code{\link{getFilterValues}}.
#'
#' \itemize{
#'   \item{task.desc [\code{\link{TaskDesc}}]}{Task description.}
#'   \item{data [named \code{list}], Filter method is the name}
#'   \itemize{
#'     \item{data [\code{data.frame}]}{Has columns: \code{name} = Names of features;
#'     \code{val} = Feature importance values; \code{type} = Feature column type.}
#' }
#' }
#' @name FilterValues
#' @rdname FilterValues
#' @family filter
NULL
#' @export
print.FilterValues = function(x, ...) {
  catf("FilterValues:")
  catf("Task: %s", x$task.desc$id)
  catf("Method: %s", names(x$data))
  print(lapply(x$data, head))
}
