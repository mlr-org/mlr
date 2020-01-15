#' @title Fuse learner with a feature filter method.
#'
#' @description Fuses a base learner with a filter method. Creates a learner
#' object, which can be used like any other learner object. Internally uses
#' [filterFeatures] before every model fit.
#'
#' @template arg_learner
#' @param fw.method (`character(1)`)\cr
#'   Filter method. See [listFilterMethods].
#'   Default is \dQuote{randomForestSRC_importance}.
#' @param fw.base.methods (`character(1)`)\cr
#'   Simple Filter methods for ensemble filters. See [listFilterMethods]. Can
#'   only be used in combination with ensemble filters. See
#'   [listFilterEnsembleMethods].
#' @param fw.perc (`numeric(1)`)\cr
#'   If set, select `fw.perc`*100 top scoring features. Mutually exclusive with
#'   arguments `fw.abs`, `fw.threshold` and `fw.fun.
#' @param fw.abs (`numeric(1)`)\cr
#'   If set, select `fw.abs` top scoring features.
#'   Mutually exclusive with arguments `fw.perc`, `fw.threshold` and `fw.fun`.
#' @param fw.threshold (`numeric(1)`)\cr
#'   If set, select features whose score exceeds `fw.threshold`. Mutually
#'   exclusive with arguments `fw.perc`, `fw.abs` and `fw.fun`.
#' @param fw.fun (`function)`)\cr
#'   If set, select features via a custom thresholding function, which must
#'   return the number of top scoring features to select. Mutually exclusive
#'   with arguments `fw.perc`, `fw.abs` and `fw.threshold`.
#' @param fw.fun.args (any)\cr
#'   Arguments passed to the custom thresholding function
#' @param fw.mandatory.feat ([character])\cr
#'   Mandatory features which are always
#'   included regardless of their scores
#' @param cache (`character(1)` | [logical])\cr
#'   Whether to use caching during
#'   filter value creation. See details.
#' @param ... (any)\cr
#'   Additional parameters passed down to the filter. If you are using more than
#'   one filter method, you need to pass the arguments in a named list via
#'   `more.args`. For example `more.args = list("FSelectorRcpp_information.gain"
#'   = list(equal = TRUE))`.
#'
#' @section Caching:
#'   If `cache = TRUE`, the default mlr cache directory is used to cache filter
#'   values. The directory is operating system dependent and can be checked with
#'   `getCacheDir()`. Alternatively a custom directory can be passed to store
#'   the cache. The cache can be cleared with `deleteCacheDir()`. Caching is
#'   disabled by default. Care should be taken when operating on large clusters
#'   due to possible write conflicts to disk if multiple workers try to write
#'   the same cache at the same time.
#'
#' @template ret_learner
#' @export
#' @family filter
#' @family wrapper
#'
#' @details
#' If `ensemble = TRUE`, ensemble feature selection using all methods specified
#' in `fw.method` is performed. At least two methods need to be selected.
#'
#' After training, the selected features can be retrieved with
#' [getFilteredFeatures].
#'
#' Note that observation weights do not influence the filtering and are simply
#' passed down to the next learner.
#'
#' @examples
#' \donttest{
#' task = makeClassifTask(data = iris, target = "Species")
#' lrn = makeLearner("classif.lda")
#' inner = makeResampleDesc("Holdout")
#' outer = makeResampleDesc("CV", iters = 2)
#' lrn = makeFilterWrapper(lrn, fw.perc = 0.5)
#' mod = train(lrn, task)
#' print(getFilteredFeatures(mod))
#' # now nested resampling, where we extract the features that the filter method selected
#' r = resample(lrn, task, outer, extract = function(model) {
#'   getFilteredFeatures(model)
#' })
#' print(r$extract)
#'
#' # usage of an ensemble filter
#' lrn = makeLearner("classif.lda")
#' lrn = makeFilterWrapper(lrn, fw.method = "E-Borda",
#'   fw.base.methods = c("FSelectorRcpp_gain.ratio", "FSelectorRcpp_information.gain"),
#'   fw.perc = 0.5)
#' r = resample(lrn, task, outer, extract = function(model) {
#'   getFilteredFeatures(model)
#' })
#' print(r$extract)
#'
#' # usage of a custom thresholding function
#' biggest_gap = function(values, diff) {
#'   gap_size = 0
#'   gap_location = 0
#'
#'   for (i in (diff + 1):length(values)) {
#'     gap = values[[i - diff]] - values[[i]]
#'     if (gap > gap_size) {
#'       gap_size = gap
#'       gap_location = i - 1
#'     }
#'   }
#'   return(gap_location)
#' }
#'
#' lrn = makeLearner("classif.lda")
#' lrn = makeFilterWrapper(lrn, fw.method = "randomForestSRC_importance",
#'   fw.fun = biggest_gap, fw.fun.args = list("diff" = 1))
#' r = resample(lrn, task, outer, extract = function(model) {
#'   getFilteredFeatures(model)
#' })
#' print(r$extract)
#' }
makeFilterWrapper = function(learner, fw.method = "randomForestSRC_importance",
  fw.base.methods = NULL, fw.perc = NULL, fw.abs = NULL, fw.threshold = NULL,
  fw.fun = NULL, fw.fun.args = NULL, fw.mandatory.feat = NULL, cache = FALSE, ...) {

  learner = checkLearner(learner)

  ### some checks

  if (length(fw.method) >= 2) {
    stopf("You passed more than one filter method. If you want to tune over filter methods, please use 'tuneParams()' or 'resample()'.")
  }

  # fw.method must exists
  assertChoice(fw.method, choices = append(ls(.FilterRegister), ls(.FilterEnsembleRegister)))
  filter = .FilterRegister[[fw.method]]

  # check if base-methods are supplied along with an ensemble method
  # (filter == NULL if an ensemble filter is supplied)
  if (is.null(filter)) {
    filter = .FilterEnsembleRegister[[fw.method]]
    # check if ONLY base-methods are supplied along with an ensemble method
    lapply(fw.base.methods, function(x) assertChoice(x, choices = ls(.FilterRegister)))
  }

  # if fw.base.methods are supplied, fw.method must be an ensemble filter
  if (!is.null(fw.base.methods)) {
    assertChoice(fw.method, choices = ls(.FilterEnsembleRegister))
  }

  ddd = list(...)
  assertList(ddd, names = "named")

  lrn = makeBaseWrapper(
    id = stri_paste(learner$id, "filtered", sep = "."),
    type = learner$type,
    next.learner = learner,
    package = filter$pkg,
    par.set = makeParamSet(
      makeDiscreteLearnerParam(id = "fw.method", values = append(ls(.FilterRegister), ls(.FilterEnsembleRegister))),
      makeDiscreteLearnerParam(id = "fw.base.methods", values = ls(.FilterRegister)),
      makeNumericLearnerParam(id = "fw.perc", lower = 0, upper = 1),
      makeIntegerLearnerParam(id = "fw.abs", lower = 0),
      makeNumericLearnerParam(id = "fw.threshold"),
      makeFunctionLearnerParam(id = "fw.fun"),
      makeUntypedLearnerParam(id = "fw.fun.args", default = NULL),
      makeUntypedLearnerParam(id = "fw.mandatory.feat")
    ),
    par.vals = filterNull(list(fw.method = fw.method,
      fw.base.methods = fw.base.methods, fw.perc = fw.perc,
      fw.abs = fw.abs, fw.threshold = fw.threshold,
      fw.fun = fw.fun, fw.fun.args = fw.fun.args,
      fw.mandatory.feat = fw.mandatory.feat)),
    learner.subclass = "FilterWrapper", model.subclass = "FilterModel",
    cache = cache)
  lrn$more.args = ddd
  lrn
}

#' @export
trainLearner.FilterWrapper = function(.learner, .task, .subset = NULL,
  .weights = NULL, fw.method = "randomForestSRC_importance",
  fw.base.methods = NULL, fw.perc = NULL, fw.abs = NULL, fw.threshold = NULL,
  fw.fun = NULL, fw.fun.args = NULL, fw.mandatory.feat = NULL, ...) {
  .task = subsetTask(.task, subset = .subset)
  .task = do.call(filterFeatures, c(list(task = .task, method = fw.method,
    base.methods = fw.base.methods,
    perc = fw.perc, abs = fw.abs, threshold = fw.threshold,
    fun = fw.fun, fun.args = fw.fun.args,
    mandatory.feat = fw.mandatory.feat,
    cache = .learner$cache), .learner$more.args))
  m = train(.learner$next.learner, .task, weights = .weights)
  makeChainModel(next.model = m, cl = "FilterModel")
}


#' @export
predictLearner.FilterWrapper = function(.learner, .model, .newdata, ...) {
  features = getFilteredFeatures(.model)
  NextMethod(.newdata = .newdata[, features, drop = FALSE])
}

#' Returns the filtered features.
#'
#' @param model ([WrappedModel])\cr
#'   Trained Model created with [makeFilterWrapper].
#' @return ([character]).
#' @export
#' @family filter
getFilteredFeatures = function(model) {
  UseMethod("getFilteredFeatures")
}

#' @export
getFilteredFeatures.default = function(model) {
  if (is.null(model$learner.model$next.model)) {
    NULL
  } else {
    getFilteredFeatures(model$learner.model$next.model)
  }
}

#' @export
getFilteredFeatures.FilterModel = function(model) {
  model$learner.model$next.model$features
}
