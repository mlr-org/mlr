.FilterEnsembleRegister = new.env()  # nolint

#' Create an ensemble feature filter.
#'
#' Creates and registers custom ensemble feature filters. Implemented ensemble filters
#' can be listed with [listFilterEnsembleMethods]. Additional
#' documentation for the `fun` parameter specific to each filter can
#' be found in the description.
#'
#' @importFrom dplyr ungroup group_by arrange mutate summarise select bind_rows
#' @importFrom rlang .data
#' @param name (`character(1)`)\cr
#'  Identifier for the filter.
#' @param basal.methods the basal filter methods which the ensemble method
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
makeFilterEnsemble = function(name = "E-min",
  basal.methods = c("randomForestSRC.rfsrc", "variance"),
  desc = NULL, fun = NULL) {

  assertString(name)
  assertString(desc)
  assertFunction(fun, c("task", "basal.methods"))
  obj =  makeS3Obj("FilterEnsemble",
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
#' @importFrom tibble tibble
#' @param desc (`logical(1)`)\cr
#'  Provide more detailed information about filters.
#'  Default is `TRUE`.
#' @return ([tibble]).
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

  if (desc)
    df$desc = description
  res = setRowNames(sortByCol(df, "id"), NULL)
  addClasses(res, "FilterMethodsList")
}

#' @export
print.FilterEnsembleMethodsList = function(x, len = 40, ...) {
  if (!is.null(x$desc))
    x$desc = clipString(x$desc, len = len)
  NextMethod()
}

#' @export
print.FilterEnsemble = function(x, ...) {
  catf("Filter: '%s'", x$name)
}

# E-min ----------------
#' Minimum redundancy, maximum relevance filter \dQuote{mrmr} computes the
#' mutual information between the target and each individual feature minus the
#' average mutual information of previously selected features and this feature
#' using the \pkg{mRMRe} package.
#'
#' @rdname makeFilter
#' @name makeFilter
makeFilterEnsemble(
  name = "E-min",
  desc = "Minimum ensemble filter. Takes the best minimum value across all basal filter methods for each feature.",
  basal.methods = NULL,
  fun = function(task, basal.methods, nselect, more.args, ...) {

    # calculate basal filters here
    fval = generateFilterValuesData(task, method = basal.methods,
        nselect = nselect, more.args = ...)

    # rank basal filters by method
    fval_all_ranked_simple = fval$data %>%
      group_by(.data$method) %>%
      arrange(.data$value) %>%
      mutate(rank = 1:length(.data$value)) %>%
      ungroup()

    # calculate ensemble filter
    fval_ens = fval_all_ranked_simple %>%
      group_by(.data$name) %>%
      summarise(value = min(rank)) %>%
      mutate(type = fval$data$type[1:length(unique(fval$data$name))]) %>%
      mutate(method = "E-min")

    # merge ensemble and basal filters into one tbl
    bind_rows(fval_all_ranked_simple, fval_ens) %>%
      select(-rank)
  }
)

# E-mean ----------------
#' Minimum redundancy, maximum relevance filter \dQuote{mrmr} computes the
#' mutual information between the target and each individual feature minus the
#' average mutual information of previously selected features and this feature
#' using the \pkg{mRMRe} package.
#'
#' @rdname makeFilter
#' @name makeFilter
makeFilterEnsemble(
  name = "E-mean",
  desc = "Mean ensemble filter. Takes the mean across all basal filter methods for each feature.",
  basal.methods = NULL,
  fun = function(task, basal.methods, nselect, more.args, ...) {

    # calculate basal filters here
    fval = generateFilterValuesData(task, method = basal.methods,
        nselect = nselect, more.args = ...)

    # rank basal filters by method
    fval_all_ranked_simple = fval$data %>%
      group_by(.data$method) %>%
      arrange(.data$value) %>%
      mutate(rank = 1:length(.data$value)) %>%
      ungroup()

    # calculate ensemble filter
    fval_ens = fval_all_ranked_simple %>%
      group_by(.data$name) %>%
      summarise(value = min(rank)) %>%
      mutate(type = fval$data$type[1:length(unique(fval$data$name))]) %>%
      mutate(method = "E-mean")

    # merge ensemble and basal filters into one tbl
    bind_rows(fval_all_ranked_simple, fval_ens) %>%
      select(-rank)
  }
)

# E-max ----------------
#' Minimum redundancy, maximum relevance filter \dQuote{mrmr} computes the
#' mutual information between the target and each individual feature minus the
#' average mutual information of previously selected features and this feature
#' using the \pkg{mRMRe} package.
#'
#' @rdname makeFilter
#' @name makeFilter
makeFilterEnsemble(
  name = "E-max",
  desc = "Maximum ensemble filter. Takes the best maximum value across all basal filter methods for each feature.",
  basal.methods = NULL,
  fun = function(task, basal.methods, nselect, more.args, ...) {

    # calculate basal filters here
    fval = generateFilterValuesData(task, method = basal.methods,
                                    nselect = nselect, more.args = ...)

    # rank basal filters by method
    fval_all_ranked_simple = fval$data %>%
      group_by(.data$method) %>%
      arrange(.data$value) %>%
      mutate(rank = 1:length(.data$value)) %>%
      ungroup()

    # calculate ensemble filter
    fval_ens = fval_all_ranked_simple %>%
      group_by(.data$name) %>%
      summarise(value = max(rank)) %>%
      mutate(type = fval$data$type[1:length(unique(fval$data$name))]) %>%
      mutate(method = "E-max")

    # merge ensemble and basal filters into one tbl
    bind_rows(fval_all_ranked_simple, fval_ens) %>%
      select(-rank)
  }
)

# E-median ----------------
#' Minimum redundancy, maximum relevance filter \dQuote{mrmr} computes the
#' mutual information between the target and each individual feature minus the
#' average mutual information of previously selected features and this feature
#' using the \pkg{mRMRe} package.
#'
#' @rdname makeFilter
#' @name makeFilter
makeFilterEnsemble(
  name = "E-median",
  desc = "Median ensemble filter. Takes the median across all basal filter methods for each feature.",
  basal.methods = NULL,
  fun = function(task, basal.methods, nselect, more.args, ...) {

    # calculate basal filters here
    fval = generateFilterValuesData(task, method = basal.methods,
                                    nselect = nselect, more.args = ...)

    # rank basal filters by method
    fval_all_ranked_simple = fval$data %>%
      group_by(.data$method) %>%
      arrange(.data$value) %>%
      mutate(rank = 1:length(.data$value)) %>%
      ungroup()

    # calculate ensemble filter
    fval_ens = fval_all_ranked_simple %>%
      group_by(.data$name) %>%
      summarise(value = median(rank)) %>%
      mutate(type = fval$data$type[1:length(unique(fval$data$name))]) %>%
      mutate(method = "E-median")

    # merge ensemble and basal filters into one tbl
    bind_rows(fval_all_ranked_simple, fval_ens) %>%
      select(-rank)
  }
)

# E-Borda ----------------
#' Minimum redundancy, maximum relevance filter \dQuote{mrmr} computes the
#' mutual information between the target and each individual feature minus the
#' average mutual information of previously selected features and this feature
#' using the \pkg{mRMRe} package.
#'
#' @rdname makeFilter
#' @name makeFilter
makeFilterEnsemble(
  name = "E-Borda",
  desc = "Borda ensemble filter. Takes the sum across all basal filter methods for each feature.",
  basal.methods = NULL,
  fun = function(task, basal.methods, nselect, more.args, ...) {

    # calculate basal filters here
    fval = generateFilterValuesData(task, method = basal.methods,
                                    nselect = nselect, more.args = ...)

    # rank basal filters by method
    fval_all_ranked_simple = fval$data %>%
      group_by(.data$method) %>%
      arrange(.data$value) %>%
      mutate(rank = 1:length(.data$value)) %>%
      ungroup()

    # calculate ensemble filter
    fval_ens = fval_all_ranked_simple %>%
      group_by(.data$name) %>%
      summarise(value = sum(rank)) %>%
      mutate(type = fval$data$type[1:length(unique(fval$data$name))]) %>%
      mutate(method = "E-Borda")

    # merge ensemble and basal filters into one tbl
    bind_rows(fval_all_ranked_simple, fval_ens) %>%
      select(-rank)
  }
)
