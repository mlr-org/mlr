#' Fuse learner with variable selection.
#'
#' Fuses a base learner with a search strategy to select variables.
#' Creates a learner object, which can be used like any other learner object,
#' but which internally uses varsel. If the train function is called on it,
#' the search strategy and resampling are invoked to select an optimal set of variables.
#' Finally, a model is fitted on the complete training data with these variables and returned.
#' See \code{\link{selectFeatures}} for more details.
#'
#' @param learner [\code{\linkS4class{Learner}} or string]\cr
#'   Learning algorithm. See \code{\link{learners}}.
#' @param resampling [\code{\linkS4class{ResampleInstance}}] or [\code{\linkS4class{ResampleDesc}}]\cr
#'   Resampling strategy to evaluate points in hyperparameter space.
#' @param measures [list of \code{\linkS4class{Measure}}]\cr
#'   Performance measures to evaluate. The first measure, aggregated by the first aggregation function is optimized during tuning, others are simply evaluated.
#' @param bit.names [character]\cr
#'   Names of bits encoding the solutions. Also defines the total number of bits in the encoding.
#'   Per default these are the feature names of the task.
#' @param bits.to.features [function(x, task)]\cr
#'   Function which transforms an integer-0-1 vector into a character vector of selected features.
#'   Per default a value of 1 in the ith bit selects the ith feature to be in the candidate solution.
#' @param control [\code{\linkS4class{VarselControl}}]
#'   Control object for search method. Also selects the optimization algorithm for feature selection.
#' @param log.fun [function(learner, task, resampling, measure, par.set, control, opt.path, x, y)]\cr
#'   Called after every hyperparameter evaluation. Default is to print performance via mlr logger.
#' @return \code{\link{Learner}}.
#' @export
makeFeatSelWrapper = function(learner, resampling, measures, bit.names, bits.to.features,
  control, show.info=TRUE) {

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
    bit.names = getTaskFeatureNames(task)
  } else {
    checkArg(bit.names, "character", na.ok=FALSE)
  }
  if (missing(bits.to.features)) {
    bits.to.features = function(x, task) binaryToFeatures(x, getTaskFeatureNames(task))
  } else {
    checkArg(bits.to.features, "function", formals=c("x", "task"))
  }
  checkArg(control, "FeatSelControl")
  checkArg(show.info, "logical", len=1L, na.ok=FALSE)
  id = paste(learner$id, "featsel", sep=".")
  x = makeOptWrapper(id, learner, resampling, measures, makeParamSet(), bit.names,
    bits.to.features, control, show.info, "FeatSelWrapper")
  # checkVarselParset(learner, par.set, bit.names, control)
  return(x)
}

#' @S3method trainLearner FeatSelWrapper
trainLearner.FeatSelWrapper = function(.learner, .task, .subset,  ...) {
  task = subsetTask(.task, .subset)
  if (length(.learner$bit.names) == 0)
    or = selectFeatures(.learner$next.learner, task, .learner$resampling, .learner$control,
      .learner$measures)
  else
    or = selectFeatures(.learner$next.learner, task, .learner$resampling, .learner$control,
      .learner$measures, .learner$bit.names, .learner$bits.to.features)
  task2 = subsetTask(task, features=or$x)
  m = train(.learner$next.learner, task2)
  x = makeChainModel(next.model=m, cl = "FeatSelModel")
  x$opt.result = or
  return(x)
}

#' @S3method predictLearner FeatSelWrapper
predictLearner.FeatSelWrapper = function(.learner, .model, .newdata, ...) {
  .newdata = .newdata[, .model$learner.model$opt.result$x, drop=FALSE]
  predictLearner(.learner$next.learner, .model$learner.model$next.model, .newdata, ...)
}

#' Returns the optimal hyperparameters and optimization path.
#'
#' @param model [\code{\link{WrappedModel}}]\cr
#'   Trained Model created with \code{\link{makeTuneWrapper}}.
#' @return [\code{\link{TuneResult}}].
#' @export
getFeatSelResult = function(model) {
  model$learner.model$opt.result
}


