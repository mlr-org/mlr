#' @title Fuse learner with a feature filter method.
#'
#' @description
#' Fuses a base learner with a filter method. Creates a learner object, which can be
#' used like any other learner object.
#' Internally uses \code{\link{filterFeatures}} before every model fit.
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
#'   Default is \dQuote{perc}.
#' @param fw.val [\code{numeric(1)}]\cr
#'   See \code{\link{filterFeatures}}.
#'   Default is 1.
#' @template ret_learner
#' @export
#' @family filter
#' @examples
#' task = makeClassifTask(data = iris, target = "Species")
#' lrn = makeLearner("classif.lda")
#' inner = makeResampleDesc("Holdout")
#' outer = makeResampleDesc("CV", iters = 2)
#' lrn = makeFilterWrapper(lrn, fw.val = 0.5)
#' mod = train(lrn, task)
#' print(getFilteredFeatures(mod))
#' # now nested resampling, where we extract the features that the filter method selected
#' r = resample(lrn, task, outer, extract = function(model) {
#'   getFilteredFeatures(model)
#' })
#' print(r$extract)
makeFilterWrapper = function(learner, fw.method = "random.forest.importance", fw.select = "perc", fw.val = 1) {
  learner = checkLearner(learner)
  pv = list()
  if (!missing(fw.method)) {
    assertChoice(fw.method, choices = listFilterMethods())
    pv$fw.method = fw.method
  }
  checkFilterArguments(select = fw.select, val = fw.val)
  if (!missing(fw.select))
    pv$fw.select = fw.select
  if (!missing(fw.val))
    pv$fw.val = fw.val
  id = paste(learner$id, "filtered", sep = ".")
  ps = makeParamSet(
    makeDiscreteLearnerParam(id = "fw.method", values = listFilterMethods()),
    makeDiscreteLearnerParam(id = "fw.select", values = c("perc", "abs", "threshold")),
    makeNumericLearnerParam(id = "fw.val")
  )
  makeBaseWrapper(id, learner, package = "FSelector", par.set = ps, par.vals = pv, cl = "FilterWrapper")
}


#' @export
trainLearner.FilterWrapper = function(.learner, .task, .subset, .weights = NULL,
  fw.method = "random.forest.importance", fw.select = "perc", fw.val = 1, ...) {

  .task = subsetTask(.task, subset = .subset)
  .task = filterFeatures(.task, method = fw.method, select = fw.select, val = fw.val)
  m = train(.learner$next.learner, .task, weights = .weights)
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
#' @family filter
getFilteredFeatures = function(model) {
  model$learner.model$next.model$features
}

