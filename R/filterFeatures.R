#' @title Filter features by thresholding filter values.
#'
#' @description
#' First, calls [generateFilterValuesData].
#' Features are then selected via `select` and `val`.
#'
#' @importFrom rlang .data
#' @importFrom dplyr arrange pull slice desc
#' @importFrom magrittr %>% %<>%
#' @template arg_task
#' @param method (`character(1)`)\cr
#'   See [listFilterMethods].
#'   Default is \dQuote{randomForestSRC_importance}.
#' @param fval ([FilterValues])\cr
#'   Result of [generateFilterValuesData].
#'   If you pass this, the filter values in the object are used for feature filtering.
#'   `method` and `...` are ignored then.
#'   Default is `NULL` and not used.
#' @param perc (`numeric(1)`)\cr
#'   If set, select `perc`*100 top scoring features.
#'   `perc = 1` means to select all features.`
#'   Mutually exclusive with arguments `abs` and `threshold`.
#' @param abs (`numeric(1)`)\cr
#'   If set, select `abs` top scoring features.
#'   Mutually exclusive with arguments `perc` and `threshold`.
#' @param threshold (`numeric(1)`)\cr
#'   If set, select features whose score exceeds `threshold`.
#'   Mutually exclusive with arguments `perc` and `abs`.
#' @param mandatory.feat ([character])\cr
#'   Mandatory features which are always included regardless of their scores
#' @param select.method If multiple methods are supplied in argument `method`,
#'   specify the method that is used for the final subsetting.
#' @param basal.methods If `method` is an ensemble filter, specify the basal
#'   filter methods which the ensemble method will use.
#' @param cache (`character(1)` | [logical])\cr
#'   Whether to use caching during filter value creation. See details.
#' @param ... (any)\cr
#'   Passed down to selected filter method.
#'
#' @section Caching:
#' If `cache = TRUE`, the default mlr cache directory is used to cache
#' filter values. The directory is operating system dependent and can be
#' checked with `getCacheDir()`.\cr
#' The default cache can be cleared with `deleteCacheDir()`.
#' Alternatively, a custom directory can be passed to store the cache.
#'
#' Note that caching is not thread safe. It will work for parallel
#' computation on many systems, but there is no guarantee.
#'
#' @template ret_task
#' @family filter
#'
#' @section Simple and ensemble filters:
#'
#' Besides passing (multiple) simple filter methods you can also pass an ensemble
#' filter method (in a list). The ensemble method will use the simple methods to
#' calculate its ranking. See `listFilterEnsembleMethods()` for available ensemble methods.
#'
#' @examples
#' # simple filter
#' filterFeatures(iris.task, method = "FSelectorRcpp_gain.ratio", abs = 2)
#' # ensemble filter
#' filterFeatures(iris.task, method = "E-min",
#'   basal.methods = c("FSelectorRcpp_gain.ratio", "FSelectorRcpp_information.gain"), abs = 2)
#' @export
filterFeatures = function(task, method = "randomForestSRC_importance", fval = NULL,
  perc = NULL, abs = NULL, threshold = NULL, mandatory.feat = NULL,
  select.method = NULL, basal.methods = NULL, cache = FALSE, ...) {

  assertClass(task, "SupervisedTask")

  # basal.methods arrive here in a list when called from 'tuneParams'.
  # we need them as a chr vec for further proc, so transforming
  if (is.list(basal.methods)) {
    basal.methods = as.character(basal.methods)
  }

  # if a list is passed to arg 'method' (ensemble)
  if (class(method) == "list") {
    ens.method = method[[1]]
    method = method[[2]]
    assertSubset(ens.method, choices = ls(.FilterEnsembleRegister), empty.ok = FALSE)
    if (length(method) == 1) {
      warningf("You only passed one basal filter method to an ensemble filter. Please use at least two basal filter methods to have a voting effect.")
    }
  } else {
    assertChoice(method, choices = append(ls(.FilterRegister), ls(.FilterEnsembleRegister)))
  }

  method %in% ls(.FilterEnsembleRegister)

  # if an ensemble method is not passed as a list but via 'basal.methods' + 'method'
  if (method %in% ls(.FilterEnsembleRegister) && !is.null(basal.methods)) {
    if (length(basal.methods) == 1) {
      warningf("You only passed one basal filter method to an ensemble filter. Please use at least two basal filter methods to have a voting effect.")
    }
    method = list(method, basal.methods)
  }

  select = checkFilterArguments(perc, abs, threshold)
  p = getTaskNFeats(task)
  nselect = switch(select,
    perc = round(perc * p),
    abs = min(abs, p),
    threshold = p
  )

  # Caching implementation: @pat-s, Nov 2018
  if (is.null(fval)) {

    if (!isFALSE(cache)) {
      requirePackages("memoise", why = "caching of filter features", default.method = "load")

      # check for user defined cache dir
      if (is.character(cache)) {
        assertString(cache)
        if (!dir.exists(cache)) {
          dir.create(cache, recursive = TRUE)
        }
        cache.dir = cache
      } else {
        assertFlag(cache)
        if (!dir.exists(rappdirs::user_cache_dir("mlr", "mlr-org"))) {
          dir.create(rappdirs::user_cache_dir("mlr", "mlr-org"))
        }
        cache.dir = rappdirs::user_cache_dir("mlr", "mlr-org")
      }

      # caching calls to `generateFilterValuesData()` with the same arguments
      cache.dir = memoise::cache_filesystem(cache.dir)
      generateFVData = memoise::memoise(generateFilterValuesData, cache = cache.dir)
    } else {
      generateFVData = generateFilterValuesData
    }
    fval = generateFilterValuesData(task = task, method = method, nselect = getTaskNFeats(task), ...)$data
  } else {
    assertClass(fval, "FilterValues")
    if (!is.null(fval$method)) { ## fval is generated by deprecated getFilterValues
      colnames(fval$data)[which(colnames(fval$data) == "val")] = fval$method
      method = fval$method
      fval = fval$data[, c(1, 3, 2)]
    } else {
      methods = unique(fval$data$method)
      if (length(methods) > 1) {
        assert(method %in% methods)
      } else {
        method = methods
        fval = fval$data
      }
    }
  }

  if (all(is.na(fval$value))) {
    stopf("Filter method returned all NA values!")
  }

  if (!is.null(mandatory.feat)) {
    assertCharacter(mandatory.feat)
    if (!all(mandatory.feat %in% fval$name)) {
      stop("At least one mandatory feature was not found in the task.")
    }
    if (select != "threshold" && nselect < length(mandatory.feat)) {
      stop("The number of features to be filtered cannot be smaller than the number of mandatory features.")
    }
    #Set the the filter values of the mandatory features to infinity to always select them
    fval[fval$name %in% mandatory.feat, "value"] = Inf
  }
  if (select == "threshold")
    nselect = sum(fval[["value"]] >= threshold, na.rm = TRUE)
  # in case multiple filters have been calculated, choose which ranking is used
  # for the final subsetting
  if (length(levels(as.factor(fval$method))) >= 2) {
    # if 'method' is an ensemble method, we always choose the ensemble method unless
    # select.method is specified specifically
    if (is.null(select.method) && !method %in% ls(.FilterEnsembleRegister)) {
      stopf("You supplied multiple filters. Please choose which should be used for the final subsetting of the features.")
    }
    if (is.null(select.method)) {
      fval %<>% dplyr::filter(method == method)
    } else {
      assertSubset(select.method, choices = unique(fval$method))
      fval %<>% dplyr::filter(method == select.method)
    }
  }
  if (nselect > 0L) {
    features = arrange(fval, method, desc(.data$value)) %>%
    slice(1:nselect) %>%
    pull(.data$name)
  } else {
    features = NULL
  }
  allfeats = getTaskFeatureNames(task)
  j = match(features, allfeats)
  features = allfeats[sort(j)]
  subsetTask(task, features = features)
}

checkFilterArguments = function(perc, abs, threshold) {

  sum.null = sum(!is.null(perc), !is.null(abs), !is.null(threshold))
  if (sum.null == 0L) {
    stop("At least one of 'perc', 'abs' or 'threshold' must be not NULL")
  }
  if (sum.null >= 2L) {
    stop("Arguments 'perc', 'abs' and 'threshold' are mutually exclusive")
  }

  if (!is.null(perc)) {
    assertNumber(perc, lower = 0, upper = 1)
    return("perc")
  }
  if (!is.null(abs)) {
    assertCount(abs)
    return("abs")
  }
  if (!is.null(threshold)) {
    assertNumber(threshold)
    return("threshold")
  }
}
