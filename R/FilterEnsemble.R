.FilterEnsembleRegister = new.env() # nolint

#' Create an ensemble feature filter.
#'
#' Creates and registers custom ensemble feature filters. Implemented ensemble filters
#' can be listed with [listFilterEnsembleMethods]. Additional
#' documentation for the `fun` parameter specific to each filter can
#' be found in the description.
#'
#' @param name (`character(1)`)\cr
#'  Identifier for the filter.
#' @param base.methods the base filter methods which the ensemble method
#'   will use.
#' @param desc (`character(1)`)\cr
#'  Short description of the filter.
#' @param fun (`function(task, nselect, ...`)\cr
#'  Function which takes a task and returns a named numeric vector of scores,
#'  one score for each feature of `task`.
#'  Higher scores mean higher importance of the feature.
#'  At least `nselect` features must be calculated, the remaining may be
#'  set to `NA` or omitted, and thus will not be selected.
#'  the original order will be restored if necessary.
#' @return Object of class \dQuote{FilterEnsemble}.
#' @export
#' @family filter
makeFilterEnsemble = function(name, base.methods, desc, fun) {

  assertString(name)
  assertString(desc)
  assertFunction(fun, c("task", "base.methods"))

  ### calculate ensemble filter
  obj = makeS3Obj("FilterEnsemble",
    name = name,
    desc = desc,
    fun = fun
  )

  .FilterEnsembleRegister[[name]] = obj
  obj
}


#' List ensemble filter methods.
#'
#' Returns a subset-able dataframe with filter information.
#'
#' @param desc (`logical(1)`)\cr
#'  Provide more detailed information about filters.
#'  Default is `TRUE`.
#' @return ([data.frame]).
#' @export
#' @family filter
listFilterEnsembleMethods = function(desc = TRUE) {

  tag2df = function(tags, prefix = "") {
    unique.tags = sort(unique(unlist(tags)))
    res = asMatrixRows(lapply(tags, "%in%", x = unique.tags))
    colnames(res) = stri_paste(prefix, unique.tags)
    rownames(res) = NULL
    as.data.frame(res)
  }
  assertFlag(desc)

  filters = as.list(.FilterEnsembleRegister)
  df = data.frame(
    id = names(filters)
  )

  description = extractSubList(filters, "desc")

  if (desc) {
    df$desc = description
  }
  res = setRowNames(sortByCol(df, "id"), NULL)
  addClasses(res, "FilterMethodsList")
  return(res)
}

#' @export
print.FilterEnsembleMethodsList = function(x, len = 40, ...) {
  if (!is.null(x$desc)) {
    x$desc = clipString(x$desc, len = len)
  }
  NextMethod()
}

#' @export
print.FilterEnsemble = function(x, ...) {
  catf("Filter: '%s'", x$name)
}

# E-min ----------------
#' Minimum ensemble filter. Takes the best minimum value across all base filter
#' methods for each feature.
#'
#' @rdname makeFilter
#' @name makeFilter
makeFilterEnsemble(
  name = "E-min",
  desc = "Minimum ensemble filter. Takes the best minimum value across all base filter methods for each feature.",
  base.methods = NULL,
  fun = function(task, base.methods, nselect, more.args) {

    fval.all.ranked = rankBaseFilters(task = task, method = base.methods,
      nselect = nselect, more.args = more.args)

    ### calculate ensemble filter

    # group by "name" and summarize the minimum of "rank"
    fval.ens = aggregate(fval.all.ranked$rank,
      by = list(fval.all.ranked$name), FUN = min)
    colnames(fval.ens) = c("name", "value")

    # add columns "type" and "method"
    fval.ens$type = fval.all.ranked$type[1:length(unique(fval.all.ranked$name))]
    fval.ens$filter = "E-min"

    # merge filters
    fval.ens = mergeFilters(fval.all.ranked, fval.ens)

    return(fval.ens)

  }
)

# E-mean ----------------
#' Mean ensemble filter. Takes the mean across all base filter methods for each feature.
#'
#' @rdname makeFilter
#' @name makeFilter
makeFilterEnsemble(
  name = "E-mean",
  desc = "Mean ensemble filter. Takes the mean across all base filter methods for each feature.",
  base.methods = NULL,
  fun = function(task, base.methods, nselect, more.args) {

    fval.all.ranked = rankBaseFilters(task = task, method = base.methods,
      nselect = nselect, more.args = more.args)

    ### calculate ensemble filter

    # group by "name" and summarize the minimum of "rank"
    fval.ens = aggregate(fval.all.ranked$rank,
      by = list(fval.all.ranked$name), FUN = mean)
    colnames(fval.ens) = c("name", "value")

    # add columns "type" and "method"
    fval.ens$type = fval.all.ranked$type[1:length(unique(fval.all.ranked$name))]
    fval.ens$filter = "E-mean"

    # merge filters
    fval.ens = mergeFilters(fval.all.ranked, fval.ens)

    return(fval.ens)

  }
)

# E-max ----------------
#' Maximum ensemble filter. Takes the best maximum value across all base filter
#' methods for each feature.
#'
#' @rdname makeFilter
#' @name makeFilter
makeFilterEnsemble(
  name = "E-max",
  desc = "Maximum ensemble filter. Takes the best maximum value across all base filter methods for each feature.",
  base.methods = NULL,
  fun = function(task, base.methods, nselect, more.args) {

    fval.all.ranked = rankBaseFilters(task = task, method = base.methods,
      nselect = nselect, more.args = more.args)

    ### calculate ensemble filter

    # group by "name" and summarize the maximum of "rank"
    fval.ens = aggregate(fval.all.ranked$rank,
      by = list(fval.all.ranked$name), FUN = max)
    colnames(fval.ens) = c("name", "value")

    # add columns "type" and "method"
    fval.ens$type = fval.all.ranked$type[1:length(unique(fval.all.ranked$name))]
    fval.ens$filter = "E-max"

    # merge filters
    fval.ens = mergeFilters(fval.all.ranked, fval.ens)

    return(fval.ens)
  }
)

# E-median ----------------
#' Median ensemble filter. Takes the median across all base filter methods for
#' each feature.
#'
#' @rdname makeFilter
#' @name makeFilter
makeFilterEnsemble(
  name = "E-median",
  desc = "Median ensemble filter. Takes the median across all base filter methods for each feature.",
  base.methods = NULL,
  fun = function(task, base.methods, nselect, more.args) {

    fval.all.ranked = rankBaseFilters(task = task, method = base.methods,
      nselect = nselect, more.args = more.args)

    ### calculate ensemble filter

    # group by "name" and summarize the median of "rank"
    fval.ens = aggregate(fval.all.ranked$rank,
      by = list(fval.all.ranked$name), FUN = median)
    colnames(fval.ens) = c("name", "value")

    # add columns "type" and "method"
    fval.ens$type = fval.all.ranked$type[1:length(unique(fval.all.ranked$name))]
    fval.ens$filter = "E-median"

    # merge filters
    fval.ens = mergeFilters(fval.all.ranked, fval.ens)

    return(fval.ens)
  }
)

# E-Borda ----------------
#' Borda ensemble filter. Takes the sum across all base filter methods for each
#' feature.
#'
#' @rdname makeFilter
#' @name makeFilter
makeFilterEnsemble(
  name = "E-Borda",
  desc = "Borda ensemble filter. Takes the sum across all base filter methods for each feature.",
  base.methods = NULL,
  fun = function(task, base.methods, nselect, more.args) {

    if (length(unique(base.methods)) == 1L) {
      stopf("Sampling without replacement is currently not supported for simple filter methods. Please use `makeDiscreteParam()` instead of `makeDiscreteVectorParam()`.")
    }

    fval.all.ranked = rankBaseFilters(task = task, method = base.methods,
      nselect = nselect, more.args = more.args)

    ### calculate ensemble filter

    # group by "name" and summarize the minimum of "rank"
    fval.ens = aggregate(fval.all.ranked$rank,
      by = list(fval.all.ranked$name), FUN = sum)
    colnames(fval.ens) = c("name", "value")

    # add columns "type" and "method"
    fval.ens$type = fval.all.ranked$type[1:length(unique(fval.all.ranked$name))]
    fval.ens$filter = "E-Borda"

    # merge filters
    fval.ens = mergeFilters(fval.all.ranked, fval.ens)

    return(fval.ens)
  }
)


# rank base filters -------------------------------------------------------
# helper fun to calculate and rank base filters for ensemble filters

rankBaseFilters = function(task, method = method,
  nselect = nselect, more.args = more.args) {

  # calculate base filters here
  fval.calc = generateFilterValuesData(task, method = method,
    nselect = nselect, more.args = more.args)

  # rank base filters by method
  value = NULL # due to NSE notes in R CMD check
  fval.all.ranked = fval.calc$data[, rank := frank(value,
    ties.method = "first"), by = filter]
  setorderv(fval.all.ranked, c("filter", "rank"))

  return(fval.all.ranked)

}

# merge base and ensemble filters ------------------------------------------------------
# helper fun to merge base and ensemble filters

mergeFilters = function(simple_filters, ensemble_filters) {
  # merge ensemble and base filters
  simple_filters$rank = NULL
  all.filters = rbind(simple_filters, ensemble_filters)

  return(all.filters)
}
