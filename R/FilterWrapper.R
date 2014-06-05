#' @title Fuse learner with a feature filter method.
#'
#' @description
#' Fuses a base learner with a filter method. Creates a learner object, which can be
#' used like any other learner object.
#' Internally uses \code{\link{filterFeatures}} before every model fit.
#'
#' Features are selected via one of: \code{fw.perc}, \code{fw.n} or \code{fw.threshold}.
#'
#' After training, the selected features can be retrieved with
#' \code{\link{getFilteredFeatures}}.
#'
#' Note that observation weights do not influence the filtering and are simply passed
#' down to the next learner.
#'
#' @template arg_learner
#' @param fw.method [\code{character(1)}]\cr
#'   See \code{\link{getFilterValues}}.
#'   Default is \dQuote{random.forest.importance}.
#' @param fw.select [\code{character(1)}]\cr
#'   See \code{\link{filterFeatures}}.
#' @param fw.val [\code{numeric(1)}]\cr
#'   See \code{\link{filterFeatures}}.
#' @template ret_learner
#' @export
#' @examples
#' task = makeClassifTask(data = iris, target = "Species")
#' lrn = makeLearner("classif.lda")
#' inner = makeResampleDesc("Holdout")
#' outer = makeResampleDesc("CV", iters = 2)
#' lrn = makeFilterWrapper(lrn, val = 0.5)
#' mod = train(lrn, task)
#' print(getFilteredFeatures(mod))
#' # now nested resampling, where we extract the features that the filter method selected
#' r = resample(lrn, task, outer, extract = function(model) {
#'   getFilteredFeatures(model)
#' })
#' print(r$extract)
makeFilterWrapper = function(learner, fw.method = "random.forest.importance", fw.select = "perc", fw.val) {
  learner = checkLearner(learner)
  checkFilterArguments(method = fw.method, select = fw.select, val = fw.val)
  id = paste(learner$id, "filtered", sep = ".")
  ps = makeParamSet(
    makeDiscreteLearnerParam(id = "fw.method", values = getFilterMethods()),
    makeDiscreteLearnerParam(id = "fw.select", values = c("perc", "abs", "threshold")),
    makeNumericLearnerParam(id = "fw.val")
  )
  pv = list(fw.method = fw.method, fw.select = fw.select, fw.val = fw.val)
  makeBaseWrapper(id, learner, package = "FSelector", par.set = ps, par.vals = pv, cl = "FilterWrapper")
  # FIXME: check that for some the inputs have to be all num. or accept error in train and NA in predict?
}


#' @export
trainLearner.FilterWrapper = function(.learner, .task, .subset, .weights = NULL, fw.method = "random.forest.importance", fw.select = "perc", fw.val, ...) {
  .task = subsetTask(.task, subset = .subset)
  # FIXME: are all filter values high = good?
  .task = filterFeatures(.task, method = fw.method, select = fw.select, val = fw.val)
  m = train(.learner$next.learner, .task, weights = .weights)
  # FIXME: enter correct objects (features, etc)
  makeChainModel(next.model = m, cl = "FilterModel")
}


#' @export
predictLearner.FilterWrapper = function(.learner, .model, .newdata, ...) {
  NextMethod(.newdata = .newdata[, .model$learner.model$next.model$features, drop = FALSE])
}


#' Returns the filtered features.
#'
#' @param model [\code{\link{WrappedModel}}]\cr
#'   Trained Model created with \code{\link{makeFilterWrapper}}.
#' @return [\code{character}].
#' @export
getFilteredFeatures = function(model) {
  model$learner.model$next.model$features
}

#' Returns a filter result
#'
#' This is a which adds a class attribute to the result of \code{\link{getFilteredFeatures}}
#' which is required for \code{\link{benchmark}}.
#'
#' @param object [\code{\link{WrappedModel}} | \code{BenchMarkResult}]\cr
#'   Trained Model created with \code{\link{makeFilterWrapper}} or benchmark result created with \code{\link{benchmark}}.
#' @return [\code{\link{FilterResult}} or list of \code{\link{FilterResult}}s].
#' @aliases FilterResult
#' @export
getFilterResult = function(object) {
  UseMethod("getFilterResult")
}

#' @export
getFilterResult.WrappedModel = function(object) {
  x = getFilteredFeatures(object)
  if (is.null(x))
    return(NULL)
  addClasses(x, "FilterResult")
}
