#' @title Fuse learner with feature selection.
#'
#' @description
#' Fuses a base learner with a search strategy to select variables.
#' Creates a learner object, which can be used like any other learner object,
#' but which internally uses \code{\link{selectFeatures}}.
#' If the train function is called on it,
#' the search strategy and resampling are invoked to select an optimal set of variables.
#' Finally, a model is fitted on the complete training data with these variables and returned.
#' See \code{\link{selectFeatures}} for more details.
#'
#' After training, the optimal features (and other related information) can be retrieved with
#' \code{\link{getFeatSelResult}}.
#'
#' @param learner [\code{\link{Learner}}]\cr
#'   The learner.
#' @param resampling [\code{\link{ResampleDesc}} | \code{\link{ResampleInstance}}]\cr
#'   Resampling strategy to evaluate feature sets. If you pass a description,
#'   it is instantiated once at the beginning by default, so all feature sets are
#'   evaluated on the same training/test sets.
#'   If you want to change that behaviour, look at \code{\link{TuneControl}}.
#' @param measures [\code{\link{Measure}} | list of \code{\link{Measure}}]\cr
#'   Performance measures to evaluate. The first measure, aggregated by the first aggregation function
#'   is optimized during feature selection, others are simply evaluated.
#' @param bit.names [\code{character}]\cr
#'   Names of bits encoding the solutions. Also defines the total number of bits in the encoding.
#'   Per default these are the feature names of the task.
#' @param bits.to.features [\code{function(x, task)}]\cr
#'   Function which transforms an integer-0-1 vector into a character vector of selected features.
#'   Per default a value of 1 in the ith bit selects the ith feature to be in the candidate solution.
#' @param control [\code{\link{FeatSelControl}}]\cr
#'   Control object for search method. Also selects the optimization algorithm for feature selection.
#' @param show.info [\code{logical(1)}]\cr
#'   Show info message after each feature set evaluation?
#'   Default is \code{TRUE}.
#' @return [\code{\link{Learner}}].
#' @export
#' @examples
#' # nested resampling with feature selection (with a pretty stupid algorithm for selection)
#' task = makeClassifTask(data = iris, target = "Species")
#' outer = makeResampleDesc("CV", iters = 2L)
#' inner = makeResampleDesc("Holdout")
#' ctrl = makeFeatSelControlRandom(maxit = 3)
#' lrn1 = makeLearner("classif.ksvm")
#' lrn2 = makeFeatSelWrapper(lrn1, resampling = inner, control = ctrl)
#' # we also extract the selected features for all iteration here
#' r = resample(lrn2, task, outer, extract = getFeatSelResult)
makeFeatSelWrapper = function(learner, resampling, measures, bit.names, bits.to.features,
  control, show.info = TRUE) {

  checkArg(learner, "Learner")
  checkArg(resampling, c("ResampleDesc", "ResampleInstance"))
  if (missing(measures)) {
    measures = default.measures(learner)
  } else {
    if (inherits(measures, "Measure"))
      measures = list(measures)
    else
      checkListElementClass(measures, "Measure")
  }
  if (missing(bit.names)) {
    bit.names = character(0)
  } else {
    checkArg(bit.names, "character", na.ok = FALSE)
  }
  if (missing(bits.to.features)) {
    bits.to.features = NULL
  } else {
    checkArg(bits.to.features, "function", formals = c("x", "task"))
  }
  checkArg(control, "FeatSelControl")
  checkArg(show.info, "logical", len = 1L, na.ok = FALSE)
  id = paste(learner$id, "featsel", sep = ".")
  x = makeOptWrapper(id, learner, resampling, measures, makeParamSet(), bit.names,
    bits.to.features, control, show.info, "FeatSelWrapper")
  # checkVarselParset(learner, par.set, bit.names, control)
  return(x)
}

#' @export
trainLearner.FeatSelWrapper = function(.learner, .task, .subset,  ...) {
  task = subsetTask(.task, .subset)
  if (length(.learner$bit.names) == 0)
    #FIXME: really look at bitnames / bits.to.features stuff and test it.
    # do we need the extra case here?
    or = selectFeatures(.learner$next.learner, task, .learner$resampling, .learner$control,
      .learner$measures)
  else
    or = selectFeatures(.learner$next.learner, task, .learner$resampling, .learner$control,
      .learner$measures, .learner$bit.names, .learner$bits.to.features)
  task = subsetTask(task, features = or$x)
  m = train(.learner$next.learner, task)
  x = makeChainModel(next.model = m, cl = "FeatSelModel")
  x$opt.result = or
  return(x)
}

#' @export
predictLearner.FeatSelWrapper = function(.learner, .model, .newdata, ...) {
  .newdata = .newdata[, .model$learner.model$opt.result$x, drop = FALSE]
  predictLearner(.learner$next.learner, .model$learner.model$next.model, .newdata, ...)
}

#' Returns the selected feature set and optimization path.
#'
#' @param model [\code{\link{WrappedModel}} | \code{BenchMarkResult}]\cr
#'   Trained Model created with \code{\link{makeFeatSelWrapper}} or benchmark result created with \code{\link{benchmark}}.
#' @return [\code{\link{FeatSelResult}} or list of \code{\link{FeatSelResult}s}].
#' @export
getFeatSelResult = function(model) {
  UseMethod("getFeatSelResult")
}

#' @export
getFeatSelResult.WrappedModel = function(model) {
  model$learner.model$opt.result
}
