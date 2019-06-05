#' @title Fuse learner with feature selection.
#'
#' @description
#' Fuses a base learner with a search strategy to select variables.
#' Creates a learner object, which can be used like any other learner object,
#' but which internally uses [selectFeatures].
#' If the train function is called on it,
#' the search strategy and resampling are invoked to select an optimal set of variables.
#' Finally, a model is fitted on the complete training data with these variables and returned.
#' See [selectFeatures] for more details.
#'
#' After training, the optimal features (and other related information) can be retrieved with
#' [getFeatSelResult].
#'
#' @template arg_learner
#' @inheritParams selectFeatures
#' @template ret_learner
#' @family featsel
#' @family wrapper
#' @noMd
#' @export
#' @examples
#' # nested resampling with feature selection (with a pretty stupid algorithm for selection)
#' outer = makeResampleDesc("CV", iters = 2L)
#' inner = makeResampleDesc("Holdout")
#' ctrl = makeFeatSelControlRandom(maxit = 1)
#' lrn = makeFeatSelWrapper("classif.ksvm", resampling = inner, control = ctrl)
#' # we also extract the selected features for all iteration here
#' r = resample(lrn, iris.task, outer, extract = getFeatSelResult)
makeFeatSelWrapper = function(learner, resampling, measures, bit.names, bits.to.features,
  control, show.info = getMlrOption("show.info")) {

  learner = checkLearner(learner)
  assert(checkClass(resampling, "ResampleDesc"), checkClass(resampling, "ResampleInstance"))
  measures = checkMeasures(measures, learner)
  if (missing(bit.names)) {
    bit.names = character(0L)
  } else {
    assertCharacter(bit.names, any.missing = FALSE)
  }
  if (missing(bits.to.features)) {
    bits.to.features = NULL
  } else {
    assertFunction(bits.to.features, args = c("x", "task"))
  }
  assertClass(control, classes = "FeatSelControl")
  assertFlag(show.info)
  id = stri_paste(learner$id, "featsel", sep = ".")
  x = makeOptWrapper(id, learner, resampling, measures, makeParamSet(), bit.names,
    bits.to.features, control, show.info, "FeatSelWrapper", "FeatSelModel")
  # checkVarselParset(learner, par.set, bit.names, control)
  return(x)
}

#' @export
trainLearner.FeatSelWrapper = function(.learner, .task, .subset = NULL, ...) {
  task = subsetTask(.task, .subset)
  if (length(.learner$bit.names) == 0) {
    # FIXME: really look at bitnames / bits.to.features stuff and test it.
    # do we need the extra case here?
    or = selectFeatures(.learner$next.learner, task, .learner$resampling,
      measures = .learner$measures, control = .learner$control, show.info = .learner$show.info)
  } else {
    or = selectFeatures(.learner$next.learner, task, .learner$resampling,
      measures = .learner$measures,
      bit.names = .learner$bit.names, bits.to.features = .learner$bits.to.features,
      control = .learner$control, show.info = .learner$show.info)
  }
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
