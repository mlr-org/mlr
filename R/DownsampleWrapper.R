#' Fuse learner with simple downsampling (subsampling).
#'
#' Creates a learner object, which can be
#' used like any other learner object.
#' It will only be trained on a subset of the original data to save computational time.
#'
#' @template arg_learner
#' @param dw.select [\code{character(1)}]\cr
#'   How to select the size of the downsampled dataset.
#'   \dQuote{perc} = select observations by percentage, \dQuote{abs} = select absolute number
#'   of observations.
#'   Default is \dQuote{perc}.
#' @param dw.val [\code{numeric(1)}]\cr
#'   Depends on \code{select}:
#'   Either a percentage from [0, 1] or a number of observations.
#' @param dw.stratify [\code{boolean(1)}]\cr
#'   Should the downsampled data be stratified according to the target classes? Default is \code{FALSE}.
#' @template ret_learner
#' @family downsample
#' @export
makeDownsampleWrapper = function(learner, dw.select = "perc", dw.val = 1, dw.stratify = FALSE) {
  checkArg(learner, "Learner")
  checkDownsampleArguments(select = dw.select, val = dw.val, stratify = dw.stratify)
  id = paste(learner$id, "downsampled", sep = ".")
  ps = makeParamSet(
    makeDiscreteLearnerParam(id = "dw.select", values = c("perc","abs")),
    makeNumericLearnerParam(id = "dw.val", lower = 0),
    makeLogicalLearnerParam(id = "dw.stratify")
  )
  pv = list(dw.select = dw.select, dw.val = dw.val, dw.stratify = dw.stratify)
  makeBaseWrapper(id, learner, package = "mlr", par.set = ps, par.vals = pv, cl = "DownsampleWrapper")
}

#' @export
trainLearner.DownsampleWrapper = function(.learner, .task, .subset, .weights = NULL, dw.select, dw.val, dw.stratify, ...) {
  .task = subsetTask(.task, .subset)
  .task = downsample(.task, select = dw.select, val = dw.val, stratify = dw.stratify)
  m = train(.learner$next.learner, .task, weights = .weights)
  makeChainModel(next.model = m, cl = "DownsampleModel")
}
