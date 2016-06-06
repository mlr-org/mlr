#' Fuse learner with simple downsampling (subsampling).
#'
#' Creates a learner object, which can be
#' used like any other learner object.
#' It will only be trained on a subset of the original data to save computational time.
#'
#' @template arg_learner
#' @param dw.perc [\code{numeric(1)}]\cr
#'   See \code{\link{downsample}}.
#'   Default is 1.
#' @param dw.stratify [\code{logical(1)}]\cr
#'   See \code{\link{downsample}}.
#'   Default is \code{FALSE}.
#' @template ret_learner
#' @family downsample
#' @family wrapper
#' @export
makeDownsampleWrapper = function(learner, dw.perc = 1, dw.stratify = FALSE) {
  learner = checkLearner(learner)
  pv = list()
  if (!missing(dw.perc)) {
    assertNumber(dw.perc, na.ok = FALSE, lower = 0, upper = 1)
    if (dw.perc == 0){
      stopf("You can't downsample %s to 0", learner$id)
    }
    pv$dw.perc = dw.perc
  }
  if (!missing(dw.stratify)) {
    assertFlag(dw.stratify)
    pv$dw.stratify = dw.stratify
  }
  id = paste(learner$id, "downsampled", sep = ".")
  ps = makeParamSet(
    makeNumericLearnerParam(id = "dw.perc", lower = 0, upper = 1, default = 1),
    makeLogicalLearnerParam(id = "dw.stratify", default = FALSE)
  )
  makeBaseWrapper(id, learner$type, learner, package = "mlr", par.set = ps, par.vals = pv,
    learner.subclass = "DownsampleWrapper", model.subclass = "DownsampleModel")
}

#' @export
trainLearner.DownsampleWrapper = function(.learner, .task, .subset, .weights = NULL,
  dw.perc = 1, dw.stratify = TRUE, ...) {

  .task = subsetTask(.task, .subset)
  .task = downsample(.task, perc = dw.perc, stratify = dw.stratify)
  m = train(.learner$next.learner, .task, weights = .weights)
  makeChainModel(next.model = m, cl = "DownsampleModel")
}
