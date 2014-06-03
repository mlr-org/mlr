#' Fuse learner with simple downsampling (subsampling).
#'
#' Creates a learner object, which can be
#' used like any other learner object.
#' It will only be trained on a subset of the original data to save eg. computational time.
#'
#' @template arg_learner
#' @param sw.percentage [\code{numeric(1)}]\cr
#'   Percentage of the observations to train the learner on.
#' @param sw.n [\code{numeric(1)}]\cr
#'   Total number of observations to train the learner on.
#' @param sw.stratify [\code{boolean(1)}]\cr
#'   Should the downsampled data be stratified according to the target classes? Default is \code{FALSE}.
#' @return [\code{\link{Learner}}].
# #' @export
makeDownsampleWrapper = function(learner, sw.percentage = NULL, sw.n = NULL, sw.stratify = FALSE) {
  checkArg(learner, "Learner")
  checkDownsampleArguments(percentage = sw.percentage, n = sw.n, stratify = sw.stratify)
  id = paste(learner$id, "downsampled", sep=".")
  ps = makeParamSet(
    makeNumericLearnerParam(id="sw.percentage", requires = expression(is.null(sw.n))),
    makeIntegerLearnerParam(id="sw.n", requires = expression(is.null(sw.percentage))),
    makeLogicalLearnerParam(id="sw.stratify")
  )
  pv = list(sw.percentage = sw.percentage, sw.n = sw.n, sw.stratify = sw.stratify)
  makeBaseWrapper(id, learner, package="mlr", par.set=ps, par.vals=pv, cl="DownsampleWrapper")
}

#' @export
trainLearner.DownsampleWrapper = function(.learner, .task, .subset, .weights = NULL, sw.percentage, sw.n, sw.stratify, ...) {
  .task = subsetTask(.task, .subset)
  .task = downsample(.task, percentage = sw.percentage, n = sw.n, stratify = sw.stratify)
  m = train(.learner$next.learner, .task, weights = .weights)
  x = makeChainModel(next.model=m, cl="UndersampleModel")
  return(x)
}
