#' @title Downsample (subsample) a task or a data.frame.
#'
#' @description
#' Decrease the observations in a `task` or a `ResampleInstance`
#' to a given percentage of observations.
#'
#' @param obj ([Task] | [ResampleInstance])\cr
#'   Input data or a `ResampleInstance`.
#' @param perc (`numeric(1)`)\cr
#'   Percentage from (0, 1).
#'   Default is 1.
#' @param stratify (`logical(1)`)\cr
#'   Only for classification:
#'   Should the downsampled data be stratified according to the target classes?
#'   Default is `FALSE`.
#' @seealso [makeResampleInstance]
#' @return ([data.frame` | [Task] | [ResampleInstance]). Same type as `obj`.
#' @family downsample
#' @export
downsample = function(obj, perc = 1, stratify = FALSE) {
  assertNumber(perc, lower = 0, upper = 1)
  assertFlag(stratify)
  UseMethod("downsample")
}

#' @export
downsample.Task = function(obj, perc = 1, stratify = FALSE) {
  rin = makeResampleInstance("Holdout", stratify = stratify, split = perc, task = obj)
  subsetTask(task = obj, subset = rin$train.inds[[1L]])
}

#' @export
downsample.ResampleInstance = function(obj, perc = 1, stratify = FALSE) {
  if (stratify) {
    stop("Stratifying is not supported for a ResampleInstance!")
  }
  obj$train.inds = lapply(obj$train.inds, function(x) sample(x, size = length(x) * perc))
  return(obj)
}
