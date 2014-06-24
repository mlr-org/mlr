#' Fuse learner with simple downsampling (subsampling).
#'
#' Creates a learner object, which can be
#' used like any other learner object.
#' It will only be trained on a subset of the original data to save computational time.
#'
#' @template arg_learner
#' @param dw.perc [\code{numeric(1)}]\cr
#'   See \code{\link{downsample}}.
#' @param dw.stratify [\code{logical(1)}]\cr
#'   See \code{\link{downsample}}.
#' @template ret_learner
#' @family downsample
#' @export
makeDownsampleWrapper = function(learner, dw.perc = 1, dw.stratify = FALSE) {
  learner = checkLearner(learner)
  checkArg(dw.perc, "numeric", len = 1L, na.ok = FALSE, lower = 0, upper = 1)
  assertLogical(dw.stratify, len = 1L, any.missing = FALSE)
  id = paste(learner$id, "downsampled", sep = ".")
  ps = makeParamSet(
    makeNumericLearnerParam(id = "dw.perc", lower = 0, upper = 1, default = 1),
    makeLogicalLearnerParam(id = "dw.stratify", default = FALSE)
  )
  pv = list(dw.perc = dw.perc, dw.stratify = dw.stratify)
  makeBaseWrapper(id, learner, package = "mlr", par.set = ps, par.vals = pv, cl = "DownsampleWrapper")
}

#' @export
trainLearner.DownsampleWrapper = function(.learner, .task, .subset, .weights = NULL, dw.perc, dw.stratify, ...) {
  .task = subsetTask(.task, .subset)
  .task = downsample(.task, perc = dw.perc, stratify = dw.stratify)
  m = train(.learner$next.learner, .task, weights = .weights)
  makeChainModel(next.model = m, cl = "DownsampleModel")
}
