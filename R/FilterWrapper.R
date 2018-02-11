#' @title Fuse learner with a feature filter method.
#'
#' @description
#' Fuses a base learner with a filter method. Creates a learner object, which can be
#' used like any other learner object.
#' Internally uses [filterFeatures] before every model fit.
#'
#' After training, the selected features can be retrieved with
#' [getFilteredFeatures].
#'
#' Note that observation weights do not influence the filtering and are simply passed
#' down to the next learner.
#'
#' @template arg_learner
#' @param fw.method (`character(1)`)\cr
#'   Filter method. See [listFilterMethods].
#'   Default is \dQuote{randomForestSRC.rfsrc}.
#' @param fw.perc (`numeric(1)`)\cr
#'   If set, select `fw.perc`*100 top scoring features.
#'   Mutually exclusive with arguments `fw.abs` and `fw.threshold`.
#' @param fw.abs (`numeric(1)`)\cr
#'   If set, select `fw.abs` top scoring features.
#'   Mutually exclusive with arguments `fw.perc` and `fw.threshold`.
#' @param fw.threshold (`numeric(1)`)\cr
#'   If set, select features whose score exceeds `fw.threshold`.
#'   Mutually exclusive with arguments `fw.perc` and `fw.abs`.
#' @param fw.mandatory.feat ([character])\cr
#'   Mandatory features which are always included regardless of their scores
#' @param ... (any)\cr
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
makeFilterWrapper = function(learner, fw.method = "randomForestSRC.rfsrc", fw.perc = NULL, fw.abs = NULL, fw.threshold = NULL, fw.mandatory.feat = NULL, ...) {
  learner = checkLearner(learner)
  assertChoice(fw.method, choices = ls(.FilterRegister))
  filter = .FilterRegister[[fw.method]]
  ddd = list(...)
  assertList(ddd, names = "named")

  lrn = makeBaseWrapper(
    id = stri_paste(learner$id, "filtered", sep = "."),
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
trainLearner.FilterWrapper = function(.learner, .task, .subset = NULL, .weights = NULL,
  fw.method = "randomForestSRC.rfsrc", fw.perc = NULL, fw.abs = NULL, fw.threshold = NULL, fw.mandatory.feat = NULL, ...) {

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
