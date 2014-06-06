#' Fuse learner with simple downsampling (subsampling).
#'
#' Creates a learner object, which can be
#' used like any other learner object.
#' It will only be trained on a subset of the original data to save computational time.
#'
#' @template arg_learner
#' @param sw.perc [\code{numeric(1)}]\cr
#'   Percentage of the observations to train the learner on.
#' @param sw.n [\code{numeric(1)}]\cr
#'   Total number of observations to train the learner on.
#' @param sw.stratify [\code{boolean(1)}]\cr
#'   Should the downsampled data be stratified according to the target classes? Default is \code{FALSE}.
#' @template ret_learner
#' @export
makeDownsampleWrapper = function(learner, sw.perc = NULL, sw.n = NULL, sw.stratify = FALSE) {
  checkArg(learner, "Learner")
  checkDownsampleArguments(perc = sw.perc, n = sw.n, stratify = sw.stratify)
  id = paste(learner$id, "downsampled", sep = ".")
  ps = makeParamSet(
    makeNumericLearnerParam(id = "sw.perc", requires = expression(is.null(sw.n)), lower = 0, upper = 1),
    makeIntegerLearnerParam(id = "sw.n", requires = expression(is.null(sw.perc)), lower = 1L),
    makeLogicalLearnerParam(id = "sw.stratify")
  )
  pv = list(sw.perc = sw.perc, sw.n = sw.n, sw.stratify = sw.stratify)
  makeBaseWrapper(id, learner, package = "mlr", par.set = ps, par.vals = pv, cl = "DownsampleWrapper")
}

#' @export
trainLearner.DownsampleWrapper = function(.learner, .task, .subset, .weights = NULL, sw.perc, sw.n, sw.stratify, ...) {
  .task = subsetTask(.task, .subset)
  .task = downsample(.task, perc = sw.perc, n = sw.n, stratify = sw.stratify)
  m = train(.learner$next.learner, .task, weights = .weights)
  makeChainModel(next.model = m, cl = "DownsampleModel")
}
