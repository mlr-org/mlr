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
#' @param fw.select [\code{character(1)}]\cr
#'   Type of thresholding. See \code{\link{filterFeatures}}.
#'   Default is \dQuote{perc}.
#' @param fw.val [\code{numeric(1)}]\cr
#'   Threshold value. See \code{\link{filterFeatures}}.
#'   Default is 1.
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
#' lrn = makeFilterWrapper(lrn, fw.val = 0.5)
#' mod = train(lrn, task)
#' print(getFilteredFeatures(mod))
#' # now nested resampling, where we extract the features that the filter method selected
#' r = resample(lrn, task, outer, extract = function(model) {
#'   getFilteredFeatures(model)
#' })
#' print(r$extract)
makeFilterWrapper = function(learner, fw.method = "rf.importance", fw.perc = NULL, fw.abs = NULL, fw.threshold = NULL, ...) {
  learner = checkLearner(learner)
  assertChoice(fw.method, choices = ls(.FilterRegister))
  filter = .FilterRegister[[fw.method]]
  select = checkFilterArguments(fw.perc, fw.abs, fw.threshold)
  ddd = list(...)
  assertList(ddd, names = "named")

  if (select == "perc") {
    param = makeNumericLearnerParam(id = "fw.perc", lower = 0, upper = 1)
    pv = list(fw.perc = fw.perc)
  } else if (select == "abs") {
    param = makeIntegerLearnerParam(id = "fw.abs", lower = 0)
    pv = list(fw.abs = fw.abs)
  } else {
    param = makeNumericLearnerParam(id = "fw.threshold")
    pv = list(fw.threshold = fw.threshold)
  }

  lrn = makeBaseWrapper(
    id = paste(learner$id, "filtered", sep = "."),
    next.learner = learner,
    package = filter$pkg,
    par.set = makeParamSet(
      makeDiscreteLearnerParam(id = "fw.method", values = ls(.FilterRegister)),
      makeDiscreteLearnerParam(id = "fw.select", values = c("perc", "abs", "threshold")),
      makeNumericLearnerParam(id = "fw.val")
    ),
    par.vals = filterNull(list(fw.method = fw.method, fw.perc = fw.perc, fw.abs = fw.abs, fw.threshold = fw.threshold)),
    cl = "FilterWrapper")
  lrn$more.args = ddd
  lrn
}

#' @export
trainLearner.FilterWrapper = function(.learner, .task, .subset, .weights = NULL,
  fw.method = "rf.importance", fw.perc = NULL, fw.abs = NULL, fw.threshold = NULL, ...) {

  .task = subsetTask(.task, subset = .subset)
  .task = do.call(filterFeatures, c(list(task = .task, method = fw.method, perc = fw.perc, abs = fw.abs, threshold = fw.threshold), .learner$more.args))
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
