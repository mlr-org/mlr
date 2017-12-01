#' @title Downsample (subsample) a task or a data.frame.
#'
#' @description
#' Decrease the observations in a \code{task} or a \code{ResampleInstance}
#' to a given percentage of observations.
#'
#' @param obj [\code{\link{Task}} | \code{\link{ResampleInstance}}]\cr
#'   Input data or a \code{ResampleInstance}.
#' @param perc [\code{numeric(1)}]\cr
#'   Percentage from [0, 1].
#'   Default is 1.
#' @param stratify [\code{logical(1)}]\cr
#'   Only for classification:
#'   Should the downsampled data be stratified according to the target classes?
#'   Default is \code{FALSE}.
#' @seealso \code{\link{makeResampleInstance}}
#' @return [\code{data.frame} | \code{\link{Task}} | \code{\link{ResampleInstance}}]. Same type as \code{obj}.
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
  if (stratify)
    stop("Stratifying is not supported for a ResampleInstance!")
  obj$train.inds = lapply(obj$train.inds, function(x) sample(x, size = length(x) * perc))
  return(obj)
}

