#' @title Calculates feature filter values.
#'
#' @description
#' Calculates numerical filter values for features.
#' For a list of features, use [listFilterMethods].
#'
#' @template arg_task
#' @param method ([character])\cr
#'   Filter method(s), see above.
#'   Default is \dQuote{randomForestSRC.rfsrc}.
#' @param nselect (`integer(1)`)\cr
#'   Number of scores to request. Scores are getting calculated for all features per default.
#' @param ... (any)\cr
#'   Passed down to selected method. Can only be use if `method` contains one element.
#' @param more.args (named [list])\cr
#'   Extra args passed down to filter methods. List elements are named with the filter
#'   `method` name the args should be passed down to.
#'   A more general and flexible option than `...`.
#'   Default is empty list.
#' @return ([FilterValues]). A `list` containing:
#'   \item{task.desc}{[[TaskDesc])\cr
#'     Task description.}
#'   \item{data}{([data.frame]) with columns:
#'     \itemize{
#'       \item `name`([character])\cr
#'         Name of feature.
#'       \item `type`([character])\cr
#'         Feature column type.
#'       \item `method`([numeric])\cr
#'         One column for each method with the feature importance values.
#'     }}
#' @family generate_plot_data
#' @family filter
#' @aliases FilterValues
#' @export
generateFilterValuesData = function(task, method = "randomForestSRC.rfsrc", nselect = getTaskNFeats(task), ..., more.args = list()) {
  assert(checkClass(task, "ClassifTask"), checkClass(task, "RegrTask"), checkClass(task, "SurvTask"))
  assertSubset(method, choices = ls(.FilterRegister), empty.ok = FALSE)
  td = getTaskDesc(task)
  filter = lapply(method, function(x) .FilterRegister[[x]])
  if (!(any(sapply(filter, function(x) !isScalarNA(filter$pkg)))))
    lapply(filter, function(x) requirePackages(x$pkg, why = "generateFilterValuesData", default.method = "load"))
  check.task = sapply(filter, function(x) td$type %nin% x$supported.tasks)
  if (any(check.task))
    stopf("Filter(s) %s not compatible with task of type '%s'",
          stri_paste("'", method[check.task], "'", collapse = ", "), td$type)

  check.feat = lapply(filter, function(x) setdiff(names(td$n.feat[td$n.feat > 0L]), x$supported.features))
  check.length = sapply(check.feat, length) > 0L
  if (any(check.length)) {
    stopf("Filter(s) %s not compatible with features of type %s respectively",
          stri_paste("'", method[check.length], "'", collapse = ", "),
          stri_paste(sapply(check.feat[check.length], function(x) stri_paste("'", x, "'", collapse = ", ")), collapse = ", and "))
  }
  assertCount(nselect)
  assertList(more.args, names = "unique", max.len = length(method))
  assertSubset(names(more.args), method)
  dot.args = list(...)
  if (length(dot.args) > 0L && length(more.args) > 0L)
    stopf("Do not use both 'more.args' and '...' here!")

  # we have dot.args, so we cannot have more.args. either complain (> 1 method) or
  # auto-setup more.args as list
  if (length(dot.args) > 0L) {
    if (length(method) == 1L)
     more.args = namedList(method, dot.args)
    else
      stopf("You use more than 1 filter method. Please pass extra arguments via 'more.args' and not '...' to filter methods!")
  }

  fn = getTaskFeatureNames(task)

  fval = lapply(filter, function(x) {
    x = do.call(x$fun, c(list(task = task, nselect = nselect), more.args[[x$name]]))
    missing.score = setdiff(fn, names(x))
    x[missing.score] = NA_real_
    x[match(fn, names(x))]
  })

  fval = do.call(cbind, fval)
  colnames(fval) = method
  types = vcapply(getTaskData(task, target.extra = TRUE)$data[fn], getClass1)
  out = data.frame(name = row.names(fval),
                   type = types,
                   fval, row.names = NULL, stringsAsFactors = FALSE)
  makeS3Obj("FilterValues",
            task.desc = td,
            data = out)
}
#' @export
print.FilterValues = function(x, ...) {
  catf("FilterValues:")
  catf("Task: %s", x$task.desc$id)
  printHead(x$data, ...)
}
#' @title Calculates feature filter values.
#'
#' @family filter
#' @family generate_plot_data
#'
#' @description
#' Calculates numerical filter values for features.
#' For a list of features, use [listFilterMethods].
#'
#' @template arg_task
#' @param method (`character(1)`)\cr
#'   Filter method, see above.
#'   Default is \dQuote{randomForestSRC.rfsrc}.
#' @param nselect (`integer(1)`)\cr
#'   Number of scores to request. Scores are getting calculated for all features per default.
#' @param ... (any)\cr
#'   Passed down to selected method.
#' @return ([FilterValues]).
#' @note `getFilterValues` is deprecated in favor of [generateFilterValuesData].
#' @family filter
#' @export
getFilterValues = function(task, method = "randomForestSRC.rfsrc", nselect = getTaskNFeats(task), ...) {
  .Deprecated("generateFilterValuesData")
  assertChoice(method, choices = ls(.FilterRegister))
  out = generateFilterValuesData(task, method, nselect, ...)
  colnames(out$data)[3] = "val"
  out$data = out$data[, c(1, 3, 2)]
  makeS3Obj("FilterValues",
            task.desc = out$task.desc,
            method = method,
            data = out$data)
}
#' Plot filter values using ggplot2.
#'
#' @family filter
#' @family generate_plot_data
#'
#' @param fvalues ([FilterValues])\cr
#'   Filter values.
#' @param sort (`character(1)`)\cr
#'   Sort features like this.
#'   \dQuote{dec} = decreasing, \dQuote{inc} = increasing, \dQuote{none} = no sorting.
#'   Default is decreasing.
#' @param n.show (`integer(1)`)\cr
#'   Number of features (maximal) to show.
#'   Default is 20.
#' @param feat.type.cols (`logical(1)`)\cr
#'   Colors for factor and numeric features.
#'   `FALSE` means no colors.
#'   Default is `FALSE`.
#' @template arg_facet_nrow_ncol
#' @template ret_gg2
#' @export
#' @examples
#' fv = generateFilterValuesData(iris.task, method = "variance")
#' plotFilterValues(fv)
plotFilterValues = function(fvalues, sort = "dec", n.show = 20L, feat.type.cols = FALSE, facet.wrap.nrow = NULL, facet.wrap.ncol = NULL) {
  assertClass(fvalues, classes = "FilterValues")
  assertChoice(sort, choices = c("dec", "inc", "none"))
  if (!(is.null(fvalues$method)))
    stop("fvalues must be generated by generateFilterValuesData, not getFilterValues, which is deprecated.")

  n.show = asCount(n.show)

  data = fvalues$data
  methods = colnames(data[, -which(colnames(data) %in% c("name", "type")), drop = FALSE])
  n.show = min(n.show, max(sapply(methods, function(x) sum(!is.na(data[[x]])))))
  data = melt(as.data.table(data), id.vars = c("name", "type"), variable = "method")

  if (sort != "none") {
    sort.mult = if (sort == "inc") 1 else -1
    setorderv(data, "value", sort.mult)
    data = data[get("method") %in% methods, head(.SD, n.show), by = "method"]
  }

  data$name = factor(data$name, levels = as.character(unique(data$name)))
  if (feat.type.cols)
    mp = aes_string(x = "name", y = "value", fill = "type")
  else
    mp = aes_string(x = "name", y = "value")
  plt = ggplot(data = data, mapping = mp)
  plt = plt + geom_bar(position = "identity", stat = "identity")
  if (length(unique(data$method)) > 1L) {
    plt = plt + facet_wrap(~ method, scales = "free_y",
      nrow = facet.wrap.nrow, ncol = facet.wrap.ncol)
    plt = plt + labs(title = sprintf("%s (%i features)",
                                              fvalues$task.desc$id,
                                              sum(fvalues$task.desc$n.feat)),
                              x = "", y = "")
  } else {
    plt = plt + labs(title = sprintf("%s (%i features), filter = %s",
                                              fvalues$task.desc$id,
                                              sum(fvalues$task.desc$n.feat),
                                              methods),
                              x = "", y = "")
  }
  plt = plt + theme(axis.text.x = element_text(angle = 45, hjust = 1))
  return(plt)
}
