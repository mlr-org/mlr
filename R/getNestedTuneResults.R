#' @title Get the tuned hyperparameter settings from a nested tuning.
#'
#' @description 
#'   After you have conducted a resampling on a wrapped learner using \code{\link{makeTuneWrapper}} with \code{resample(..., extract = getTuneresult)} this helper gives you a \code{data.frame} with the hyperparameter settings which were found to be the best.
#' 
#' @param r [\code{\link{ResampleResult}}] \cr
#'   The result of a resampling of a tuneWrapped learner.
#' @return \code{data.frame}
#'   \code{data.frame} with one column for each tuned hyperparameter and one row for each outer resampling iteration.
#' @examples
#'   ## see example of makeTuneWrapper
#' 
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
#'   After you have conducted a resampling on a wrapped learner using \code{\link{makeTuneWrapper}} with \code{resample(..., extract = getTuneresult)} this helper gives you a \code{data.frame} with all \code{opt.path}s combined by \code{rbind}.
#'   An additional column \code{iter} indicates to what resampling iteration the column belongs.
#' 
#' @param r [\code{\link{ResampleResult}}] \cr
#'   The result of a resampling of a tuneWrapped learner.
#' @return \code{data.frame}
#' @examples
#'   ## see example of makeTuneWrapper
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

