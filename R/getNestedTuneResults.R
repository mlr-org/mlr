#' @title Get the tuned hyperparameter settings from a nested tuning.
#'
#' @description
#' After you resampled a tuning wrapper (see \code{\link{makeTuneWrapper}})
#' with \code{resample(..., extract = getTuneResult)} this helper returns a \code{data.frame} with
#' the the best found hyperparameter settings for each resampling iteration.
#'
#' @param r [\code{\link{ResampleResult}}] \cr
#'   The result of resampling of a tuning wrapper.
#' @return [\code{data.frame}]. One column for each tuned hyperparameter and one row for each outer resampling iteration.
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


#' @title Get the \code{opt.path}s from each tuning step from the outer resampling.
#'
#' @description
#' After you resampled a tuning wrapper (see \code{\link{makeTuneWrapper}})
#' with \code{resample(..., extract = getTuneResult)} this helper returns a \code{data.frame} with
#' with all \code{opt.path}s combined by \code{rbind}.
#' An additional column \code{iter} indicates to what resampling iteration the row belongs.
#'
#' @param r [\code{\link{ResampleResult}}] \cr
#'   The result of resampling of a tuning wrapper.
#' @return [\code{data.frame}]. See above.
#' @family tune
#' @examples
#' # see example of makeTuneWrapper
#' @export
getNestedTuneResultsOptPathDf = function(r) {
  assertClass(r, "ResampleResult")
  assertList(r$extract)
  lapply(r$extract, assertClass, classes = "TuneResult")
  ops = extractSubList(r$extract, "opt.path", simplify = FALSE)
  op.dfs = lapply(ops, as.data.frame)
  op.dfs = lapply(seq_along(op.dfs), function(i) {
    op.dfs[[i]][,"iter"] = i
    op.dfs[[i]]
  })
  do.call(plyr::rbind.fill, op.dfs)
}

