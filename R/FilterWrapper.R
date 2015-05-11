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
#'   Filter method. See \code{\link{listFilterMethods}}.
#'   Default is \dQuote{rf.importance}.
#' @param fw.perc [\code{numeric(1)}]\cr
#'   If set, select \code{fw.perc}*100 top scoring features.
#'   Mutually exclusive with arguments \code{fw.abs} and \code{fw.threshold}.
#' @param fw.abs [\code{numeric(1)}]\cr
#'   If set, select \code{fw.abs} top scoring features.
#'   Mutually exclusive with arguments \code{fw.perc} and \code{fw.threshold}.
#' @param fw.threshold [\code{numeric(1)}]\cr
#'   If set, select features whose score exceeds \code{fw.threshold}.
#'   Mutually exclusive with arguments \code{fw.perc} and \code{fw.abs}.
#' @param fw.mandatory.feat [\code{character}]\cr
#'   Mandatory features which are always included regardless of their scores
#' @param ... [any]\cr
#'   Additional parameters passed down to the filter.
#' @template ret_learner
#' @export
#' @family filter
#' @family wrapper
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
makeFilterWrapper = function(learner, fw.method = "rf.importance", fw.perc = NULL, fw.abs = NULL, fw.threshold = NULL, fw.mandatory.feat = NULL, ...) {
  learner = checkLearner(learner)
  assertChoice(fw.method, choices = ls(.FilterRegister))
  filter = .FilterRegister[[fw.method]]
  ddd = list(...)
  assertList(ddd, names = "named")

  lrn = makeBaseWrapper(
    id = paste(learner$id, "filtered", sep = "."),
    type = learner$type,
    next.learner = learner,
    package = filter$pkg,
    par.set = makeParamSet(
      makeDiscreteLearnerParam(id = "fw.method", values = ls(.FilterRegister)),
      makeNumericLearnerParam(id = "fw.perc", lower = 0, upper = 1),
      makeIntegerLearnerParam(id = "fw.abs", lower = 0),
      makeNumericLearnerParam(id = "fw.threshold"),
      makeUntypedLearnerParam(id = "fw.mandatory.feat")
    ),
    par.vals = filterNull(list(fw.method = fw.method, fw.perc = fw.perc, fw.abs = fw.abs, fw.threshold = fw.threshold, fw.mandatory.feat = fw.mandatory.feat)),
    learner.subclass = "FilterWrapper", model.subclass = "FilterModel")
  lrn$more.args = ddd
  lrn
}

#' @export
trainLearner.FilterWrapper = function(.learner, .task, .subset, .weights = NULL,
  fw.method = "rf.importance", fw.perc = NULL, fw.abs = NULL, fw.threshold = NULL, fw.mandatory.feat = NULL, ...) {

  .task = subsetTask(.task, subset = .subset)
  .task = do.call(filterFeatures, c(list(task = .task, method = fw.method, perc = fw.perc, abs = fw.abs, threshold = fw.threshold, mandatory.feat = fw.mandatory.feat), .learner$more.args))
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
#' @param model [\code{\link{WrappedModel}}]\cr
#'   Trained Model created with \code{\link{makeFilterWrapper}}.
#' @return [\code{character}].
#' @export
#' @family filter
getFilteredFeatures = function(model) {
  model$learner.model$next.model$features
}
