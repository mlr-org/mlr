#' @title Get the tuned hyperparameter settings from a nested tuning.
#'
#' @description
#' After you resampled a tuning wrapper (see [makeTuneWrapper])
#' with `resample(..., extract = getTuneResult)` this helper returns a `data.frame` with
#' the best found hyperparameter settings for each resampling iteration.
#'
#' @param r ([ResampleResult]) \cr
#'   The result of resampling of a tuning wrapper.
#' @return ([data.frame]). One column for each tuned hyperparameter and one row for each outer resampling iteration.
#' @family tune
#' @examples
#' # see example of makeTuneWrapper
#' @export
getNestedTuneResultsX = function(r) {
  assertClass(r, "ResampleResult")
  assertList(r$extract)
  lapply(r$extract, assertClass, classes = "TuneResult")
  convertListOfRowsToDataFrame(extractSubList(r$extract, "x", simplify = FALSE))
}


#' @title Get the `opt.path`s from each tuning step from the outer resampling.
#'
#' @description
#' After you resampled a tuning wrapper (see [makeTuneWrapper])
#' with `resample(..., extract = getTuneResult)` this helper returns a `data.frame` with
#' with all `opt.path`s combined by `rbind`.
#' An additional column `iter` indicates to what resampling iteration the row belongs.
#'
#' @param r ([ResampleResult]) \cr
#'   The result of resampling of a tuning wrapper.
#' @param trafo (`logical(1)`)\cr
#'   Should the units of the hyperparameter path be converted to the
#'   transformed scale? This is only necessary when trafo was used to create
#'   the `opt.path`s. Note that `opt.path`s are always stored on the
#'   untransformed scale.
#'   Default is `FALSE`.
#' @return ([data.frame]). See above.
#' @family tune
#' @examples
#' # see example of makeTuneWrapper
#' @export
getNestedTuneResultsOptPathDf = function(r, trafo = FALSE) {
  assertClass(r, "ResampleResult")
  assertList(r$extract)
  lapply(r$extract, assertClass, classes = "TuneResult")
  assertFlag(trafo)
  ops = extractSubList(r$extract, "opt.path", simplify = FALSE)
  if (trafo) ops = lapply(ops, trafoOptPath)
  op.dfs = lapply(ops, as.data.frame)
  op.dfs = setDF(rbindlist(lapply(seq_along(op.dfs), function(i) {
    op.dfs[[i]][, "iter"] = i
    op.dfs[[i]]
  }), fill = TRUE, use.names = TRUE))
}
