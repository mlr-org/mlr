#' @title Calculates feature filter values.
#'
#' @description
#' Calculates numerical filter values for features.
#' For a list of features, use [listFilterMethods].
#' @template arg_task
#' @param method ([character] | [list])\cr
#'   Filter method(s).
#'   In case of ensemble filters the `list` notation needs to be used.
#'   See the examples for more information.
#'   Default is \dQuote{randomForestSRC_importance}.
#' @param nselect (`integer(1)`)\cr
#' Number of scores to request. Scores are getting calculated for all features
#' per default.
#' @param ... (any)\cr
#'   Passed down to selected method. Can only be use if `method` contains one
#'   element.
#' @param more.args (named [list])\cr
#'   Extra args passed down to filter methods. List elements are named with the
#'   filter `method` name the args should be passed down to.
#'   A more general and flexible option than `...`.
#'   Default is empty list.
#' @return ([FilterValues]). A `list` containing:
#'   \item{task.desc}{[[TaskDesc])\cr
#'     Task description.}
#'   \item{data}{(`data.frame`) with columns:
#'     \itemize{
#'       \item `name`([character])\cr
#'         Name of feature.
#'       \item `type`([character])\cr
#'         Feature column type.
#'       \item `method`([numeric])\cr
#'         One column for each method with the feature importance values.
#'     }}
#'
#' @section Simple and ensemble filters:
#'
#' Besides passing (multiple) simple filter methods you can also pass an
#' ensemble filter method (in a list). The ensemble method will use the simple
#' methods to calculate its ranking. See `listFilterEnsembleMethods()` for
#' available ensemble methods.
#'
#' @family generate_plot_data
#' @family filter
#' @aliases FilterValues
#' @examples
#' # two simple filter methods
#' fval = generateFilterValuesData(iris.task,
#'   method = c("FSelectorRcpp_gain.ratio", "FSelectorRcpp_information.gain"))
#' # using ensemble method "E-mean"
#' fval = generateFilterValuesData(iris.task,
#'   method = list("E-mean", c("FSelectorRcpp_gain.ratio",
#'     "FSelectorRcpp_information.gain")))
#' @export
generateFilterValuesData = function(task, method = "randomForestSRC_importance",
  nselect = getTaskNFeats(task), ..., more.args = list()) {

  # define for later checks
  ens.method = NULL

  # ensemble
  if (class(method) == "list") {
    if (method[[1]] %in% ls(.FilterEnsembleRegister)) {
      ens.method = method[[1]]
      method = method[[2]]
      if (length(method) == 1) {
        warningf("You only passed one base filter method to an ensemble filter. Please use at least two base filter methods to have a voting effect.")
      }
    }
  }

  assertSubset(unlist(method), choices = append(ls(.FilterRegister), ls(.FilterEnsembleRegister)), empty.ok = FALSE)
  filter = lapply(method, function(x) .FilterRegister[[x]])
  if (!(any(sapply(filter, function(x) !isScalarNA(filter$pkg))))) {
    req_pkg = lapply(filter, function(x) requirePackages(x$pkg, why = "generateFilterValuesData", default.method = "load"))
  }
  assert(checkClass(task, "ClassifTask"), checkClass(task, "RegrTask"), checkClass(task, "SurvTask"))
  td = getTaskDesc(task)

  filter = lapply(method, function(x) .FilterRegister[[x]])
  if (any(sapply(filter, function(x) length(x$pkg) > 0))) {
    pkgs = unlist(lapply(filter, function(x) x$pkg))
    pkgs = lapply(pkgs, function(x) requirePackages(x, why = "generateFilterValuesData", default.method = "load"))
  }
  check.task = sapply(filter, function(x) td$type %nin% x$supported.tasks)
  if (any(check.task)) {
    stopf("Filter(s) %s not compatible with task of type '%s'",
      stri_paste("'", method[check.task], "'", collapse = ", "), td$type)
  }

  check.feat = lapply(filter, function(x) setdiff(names(td$n.feat[td$n.feat > 0L]), x$supported.features))
  check.length = sapply(check.feat, length) > 0L
  if (any(check.length)) {
    stopf("Filter(s) %s not compatible with features of type %s respectively",
      stri_paste("'", method[check.length], "'", collapse = ", "),
      stri_paste(sapply(check.feat[check.length], function(x) stri_paste("'", x, "'", collapse = ", ")), collapse = ", and "))
  }
  assertCount(nselect)
  dot.args = list(...)
  if (length(dot.args) > 0L && length(more.args) > 0L) {
    stopf("Do not use both 'more.args' and '...' here!")
  }

  # we have dot.args, so we cannot have more.args. either complain (> 1 method) or
  # auto-setup more.args as list
  if (length(dot.args) > 0L) {
    if (length(method) == 1L) {
      more.args = namedList(method, dot.args)
    } else {
      stopf("You use more than 1 filter method. Please pass extra arguments via 'more.args' and not '...' to filter methods!")
    }
  } else if (length(method) == 1L && length(more.args) > 1L) { # simple filter with 1 method and more.args
    more.args = namedList(method, more.args)
  }
  assertList(more.args, names = "unique", max.len = length(method))

  fn = getTaskFeatureNames(task)

  if (!is.null(ens.method)) {

    assertSubset(ens.method, choices = ls(.FilterEnsembleRegister), empty.ok = FALSE)

    filter = lapply(ens.method, function(x) .FilterEnsembleRegister[[x]])

    out = lapply(filter, function(x) {
      x = do.call(x$fun, c(list(task = task, nselect = nselect,
        base.methods = method, more.args = more.args)))
    })

    if (length(out) == 1) {
      out = out[[1]]
    }

  } else {
    index_names = names(method)
    if (is.null(index_names)) {
      index_names = method
    }
    fval = mapply(function(x, name) {
      x = do.call(x$fun, c(list(task = task, nselect = nselect), more.args[[name]]))
      missing.score = setdiff(fn, names(x))
      x[missing.score] = NA_real_
      x[match(fn, names(x))]
    }, filter, index_names, SIMPLIFY = FALSE)
    fval = do.call(cbind, fval)
    colnames(fval) = index_names
    types = vcapply(getTaskData(task, target.extra = TRUE)$data[fn], getClass1)

    out = data.table(name = row.names(fval),
      type = types, fval, stringsAsFactors = FALSE)

    # variable.factor = FALSE has no effect
    out = melt(out, value.name = "value", measure.vars = index_names,
      variable.name = "filter")
  }

  makeS3Obj("FilterValues",
    task.desc = td,
    data = out)
}
#' @export
print.FilterValues = function(x, ...) {
  catf("FilterValues:")
  catf("Task: %s", x$task.desc$id)
  print(x$data[with(x$data, order(filter, -value)), ])
}
#' Plot filter values using ggplot2.
#'
#' @family filter
#' @family generate_plot_data
#'
#' @param fvalues ([FilterValues])\cr
#'   Filter values.
#' @param sort (`character(1)`)\cr
#'   Available options are:
#'   - `"dec"`-> descending
#'   - `"inc"` -> increasing
#'   - `"none"` -> no sorting
#'
#'   Default is decreasing.
#' @param n.show (`integer(1)`)\cr
#'   Number of features (maximal) to show.
#'   Default is to plot all features.
#' @param filter (`character(1)`)
#'   In case `fvalues` contains multiple filter methods, which method should be
#'   plotted?
#' @param feat.type.cols (`logical(1)`)\cr
#'   Whether to color different feature types (e.g. numeric | factor).
#'   Default is to use no colors (`feat.type.cols = FALSE`).
#' @template ret_gg2
#' @export
#' @examples
#' fv = generateFilterValuesData(iris.task, method = "variance")
#' plotFilterValues(fv)
plotFilterValues = function(fvalues, sort = "dec", n.show = nrow(fvalues$data),
  filter = NULL, feat.type.cols = FALSE) {

  assertClass(fvalues, classes = "FilterValues")
  assertChoice(sort, choices = c("dec", "inc", "none"))

  if (!(is.null(fvalues$filter))) {
    stop("fvalues must be generated by generateFilterValuesData, not getFilterValues, which is deprecated.")
  }

  data = fvalues$data

  if (is.null(filter) && nlevels(as.factor(data$filter)) > 1L) {
    stopf("Please supply only one filter method.")
  } else if (!is.null(filter)) {
    filter_sub = filter
    if (filter_sub %nin% levels(data$filter)) {
      stopf("Method '%s' not found among the filter methods supplied via argument 'fvalues'.", filter_sub)
    }
    data = data[filter == filter_sub, ]
  }

  # for R CMD check "undefined global variable"
  value = NULL
  name = NULL

  # we need to order both, data and the ggplot mapping
  # ggplot will reorder automatically otherwise
  if (sort == "dec") {
    # order and top_n by group: https://stackoverflow.com/a/27766055/4185785
    data = droplevels(setDT(data)[order(filter, -value, name), head(.SD, n.show), by = filter])
    mp = aes_string(x = paste0("reorder(name, -value)"), y = "value")
  } else if (sort == "inc") {
    # here we want to have the last x elements
    # order and top_n by group: https://stackoverflow.com/a/27766055/4185785
    data = setDT(data)[order(filter, value, name), tail(.SD, n.show), by = filter]
    mp = aes_string(x = paste0("reorder(name, value)"), y = "value")
  } else {
    data = setDT(data)[, head(.SD, n.show), by = filter]
    mp = aes_string(x = paste0("name"), y = "value")
  }

  # extend ggplot2 mapping
  if (feat.type.cols) {
    mp$fill = quote(type)
  }

  plt = ggplot(data = data, mapping = mp)
  plt = plt +
    geom_bar(position = "identity", stat = "identity") +
    labs(
      title = sprintf("%s (%i out of %i features), filter = %s",
        fvalues$task.desc$id,
        ifelse(n.show > sum(fvalues$task.desc$n.feat), sum(fvalues$task.desc$n.feat), n.show),
        sum(fvalues$task.desc$n.feat),
        data$filter),
      x = "", y = "") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

  return(plt)
}
