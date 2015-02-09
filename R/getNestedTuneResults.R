#' Get the tuned hyperparameter settings from a nested tuning
#'
#' After you have conducted a resampling on a wrapped learner using \code{\link{makeTuneWrapper}} with \code{resample(..., extract = getTuneresult)} this helper gives you a \code{data.frame} with the hyperparameter settings which were found to be the best.
#' 
#' @param r [\code{\link{ResampleResult}}] \cr
#'   The result of a resampling of a tuneWrapped learner.
#' @return \code{data.frame}
#' @examples
#'\dontrun{
#' ps = makeParamSet(
#'   makeDiscreteParam("C", values = 2^(-2:2)),
#'   makeDiscreteParam("sigma", values = 2^(-2:2))
#' )
#' ctrl = makeTuneControlGrid()
#' inner = makeResampleDesc("Holdout")
#' outer = makeResampleDesc("CV", iters = 5)
#' lrn = makeTuneWrapper("classif.ksvm", inner, par.set = ps, control = ctrl)
#' r = resample(lrn, iris.task, resampling = outer, extract = getTuneResult)
#' getNestedTuneResultsX(r)
#'}
#' 
#' @export 
getNestedTuneResultsX = function(r) {
  assertClass(r, "ResampleResult")
  assertList(r$extract)
  lapply(r$extract, assertClass, classes = "TuneResult")
  convertListOfRowsToDataFrame(extractSubList(r$extract, "x", simplify = FALSE))
}


#' Get the \code{opt.path}s from each tuning step from the outer resampling.
#'
#' After you have conducted a resampling on a wrapped learner using \code{\link{makeTuneWrapper}} with \code{resample(..., extract = getTuneresult)} this helper gives you a \code{data.frame} with all \code{opt.path}s combined by \code{rbind}.
#' An additional column \code{iter} indicates to what resampling iteration the column belongs.
#' 
#' @param r [\code{\link{ResampleResult}}] \cr
#'   The result of a resampling of a tuneWrapped learner.
#' @return \code{data.frame}
#' @examples
#'\dontrun{
#' ps = makeParamSet(
#'   makeDiscreteParam("C", values = 2^(-2:2)),
#'   makeDiscreteParam("sigma", values = 2^(-2:2))
#')
#' ctrl = makeTuneControlGrid()
#' inner = makeResampleDesc("Holdout")
#' outer = makeResampleDesc("CV", iters = 5)
#' lrn = makeTuneWrapper("classif.ksvm", inner, par.set = ps, control = ctrl)
#' r = resample(lrn, iris.task, resampling = outer, extract = getTuneResult)
#' getNestedTuneResultsOptPathDf(r)
#' }
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

