#' Fuse learner with simple over/undersampling for binary classification.
#'
#' Creates a learner object, which can be
#' used like any other learner object.
#' Internally uses \code{\link{oversample}} or \code{\link{undersample}} before every model fit.
#'
#' @param learner [\code{\link{Learner}}]\cr
#'   The learner.
#' @param usw.rate [\code{numeric(1)}]\cr
#'   Factor to downsample the bigger class. Must be between 0 and 1,
#'   where 1 means no downsampling, 0.5 implies reduction to 50 percent
#'   and 0 would imply reduction to 0 observations.
#' @param osw.rate [\code{numeric(1)}]\cr
#'   Factor to oversample the smaller class. Must be between 1 and \code{Inf},
#'   where 1 means no oversampling and 2 would mean doubling the class size.
#' @return [\code{\link{Learner}}].
#' @export
makeUndersampleWrapper = function(learner, usw.rate) {
  checkArg(learner, "Learner")
  checkArg(usw.rate, "numeric", len=1L, na.ok=FALSE, lower=0, upper=1)
  if (learner$type != "classif")
    stopf("Undersampling is only supported for classifiers, not for type = '%s'!", learner$type)

  id = paste(learner$id, "undersampled", sep=".")
  ps = makeParamSet(
    makeNumericLearnerParam(id="usw.rate")
  )
  pv = list(usw.rate=usw.rate)
  makeBaseWrapper(id, learner, package="mlr", par.set=ps, par.vals=pv, cl="UndersampleWrapper")
}

#' @rdname makeUndersampleWrapper
#' @export
makeOversampleWrapper = function(learner, osw.rate) {
  checkArg(learner, "Learner")
  checkArg(osw.rate, "numeric", len=1L, na.ok=FALSE, lower=1)
  if (learner$type != "classif")
    stopf("Oversampling is only supported for classifiers, not for type = '%s'!", learner$type)

  id = paste(learner$id, "overrsampled", sep=".")
  ps = makeParamSet(
    makeNumericLearnerParam(id="osw.rate")
  )
  pv = list(osw.rate=osw.rate)
  makeBaseWrapper(id, learner, package="mlr", par.set=ps, par.vals=pv, cl="OversampleWrapper")
}

#' @export
trainLearner.UndersampleWrapper = function(.learner, .task, .subset, usw.rate, ...) {
  .task = subsetTask(.task, .subset)
  .task = undersample(.task, rate=usw.rate)
  m = train(.learner$next.learner, .task)
  x = makeChainModel(next.model=m, cl="UndersampleModel")
  return(x)
}

#' @export
trainLearner.OversampleWrapper = function(.learner, .task, .subset, osw.rate, ...) {
  .task = subsetTask(.task, .subset)
  .task = oversample(.task, rate=osw.rate)
  m = train(.learner$next.learner, .task)
  x = makeChainModel(next.model=m, cl="OversampleModel")
  return(x)
}

