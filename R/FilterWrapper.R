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
#' @template arg_learner
#' @param fw.method [\code{character(1)}]\cr
#'   Filter method. Available are:
#'   linear.correlation, rank.correlation, information.gain, gain.ratio, symmetrical.uncertainty,
#'   chi.squared, random.forest.importance, relief, oneR
#'   Default is random.forest.importance.
#' @param fw.threshold [\code{numeric(1)}]\cr
#'   Information value as to be greater then the threshold. Default is 0.
#' @param fw.n [\code{integer(1)}]\cr
#'   Number of features ordered by the information value to select.
#'   This can decrease the number of features after threasholding.
#' @param fw.percentage [\code{numeric(1)}]\cr
#'   Alternatively to \code{n} you can give a relative number of features.
#' @return [\code{\link{Learner}}].
#' @export
#' @examples
#' task = makeClassifTask(data = iris, target = "Species")
#' lrn = makeLearner("classif.lda")
#' inner = makeResampleDesc("Holdout")
#' outer = makeResampleDesc("CV", iters = 2)
#' lrn = makeFilterWrapper(lrn, fw.percentage = 0.5)
#' mod = train(lrn, task)
#' print(getFilteredFeatures(mod))
#' # now nested resampling, where we extract the features that the filter method selected
#' r = resample(lrn, task, outer, extract = function(model) {
#'   getFilteredFeatures(model)
#' })
#' print(r$extract)
makeFilterWrapper = function(learner, fw.method = "random.forest.importance", fw.threshold = 0, fw.n = NULL, fw.percentage = NULL) {
  learner = checkLearner(learner)
  meths = filter.methods #defined in filterFeatures.R
  checkFilterArguments(method = fw.method, threshold = fw.threshold, n = fw.n, percentage = fw.percentage)
  id = paste(learner$id, "filtered", sep = ".")
  ps = makeParamSet(
    makeDiscreteLearnerParam(id = "fw.method", values = meths),
    makeNumericLearnerParam(id = "fw.threshold"),
    makeIntegerLearnerParam(id = "fw.n", requires = expression(is.null(fw.percentage))), #FIXME: Is that the correct way for an XOR?
    makeNumericLearnerParam(id = "fw.percentage", requires = expression(is.null(fw.n)))
  )
  pv = list(fw.method = fw.method, fw.threshold = fw.threshold, fw.n = fw.n, fw.percentage = fw.percentage)
  # FIXME: scale to 0,1
  makeBaseWrapper(id, learner, package = "FSelector", par.set = ps, par.vals = pv, cl = "FilterWrapper")
  # FIXME: check that for some the inputs have to be all num. or accept error in train and NA in predict?
}


#' @export
trainLearner.FilterWrapper = function(.learner, .task, .subset, fw.method, fw.threshold, fw.n, fw.percentage, ...) {
  .task = subsetTask(.task, subset = .subset)
  # FIXME: are all filter values high = good?
  .task = filterFeatures(.task, method = fw.method, threshold = fw.threshold, n = fw.n, percentage = fw.percentage)
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
