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
#' @param learner [\code{\link{Learner}}]\cr
#'   The learner.
#' @param fw.method [\code{character(1)}]\cr
#'   Filter method. Available are:
#'   linear.correlation, rank.correlation, information.gain, gain.ratio, symmetrical.uncertainty,
#'   chi.squared, random.forest.importance, relief, oneR
#'   Default is random.forest.importance.
#' @param fw.perc [\code{numeric(1)}]\cr
#'   Percentage of highest ranking features to select after filtering.
#'   Default is 1 ( = 100 percent).
#' @return [\code{\link{Learner}}].
#' @export
#' @examples
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
makeFilterWrapper = function(learner, fw.method = "random.forest.importance", fw.perc = 1) {
  checkArg(learner, "Learner")
  meths = c("linear.correlation", "rank.correlation", "information.gain", "gain.ratio",
    "symmetrical.uncertainty", "chi.squared", "random.forest.importance", "relief", "oneR")
  checkArg(fw.method, choices = meths)
  checkArg(fw.perc, "numeric", len = 1L, na.ok = FALSE, lower = 0, upper = 1)
  id = paste(learner$id, "filtered", sep = ".")
  ps = makeParamSet(
    makeDiscreteLearnerParam(id = "fw.method", values = meths),
    makeNumericLearnerParam(id = "fw.perc")
  )
  pv = list(fw.method = fw.method, fw.perc = fw.perc)
  # FIXME: scale to 0,1
  makeBaseWrapper(id, learner, package = "FSelector", par.set = ps, par.vals = pv, cl = "FilterWrapper")
  # FIXME: check that for some the inputs have to be all num. or accept error in train and NA in predict?
}


#' @export
trainLearner.FilterWrapper = function(.learner, .task, .subset, fw.method, fw.perc, ...) {
  .task = subsetTask(.task, subset = .subset)
  # FIXME: are all filter vales high = good?
  vals = sort(filterFeatures(.task), decreasing = TRUE)
  features = head(names(vals), round(fw.perc * length(vals)))
  # we have already subsetted obs
  .task = subsetTask(.task, features = features)
  m = train(.learner$next.learner, .task)
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
