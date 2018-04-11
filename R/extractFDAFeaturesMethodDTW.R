#' @useDynLib dtw
#' @importFrom dtw computeCM_Call
NULL
#' @title DTW kernel machine features
#'
#' @description
#' The function extracts features from functional data based on the DTW distance with a reference dataframe.
#'
#' @param dtw.refs [\code{numeric}]\cr
#' A list of reference curves
#' @param method.align [\code{character}]\cr
#' The alignment method for dtw.
#' @return [\code{data.frame}].
#' @export
#' @family fda_featextractor
extractFDADTWKernel = function(dtw.refs, method.align = dtw::asymmetric) {
  requirePackages("dtw", default.method = "attach")
  getDtwDist = function(frow, refs, step.pattern = dtw::asymmetric) {
    requireNamespace("dtw")
    list.dist = lapply(refs, function(x) {
      res = do.call(dtw, list(x = x, y = frow, step.pattern = step.pattern, keep = TRUE))
      # x is the query curve, y is the reference, keep = TRUE means we keep the cost matrix
      res$distance
    })
    unlist(list.dist)
  }
  lrn = function(data, target = NULL, col, dtw.refs, method.align) {
    assertClass(data, "data.frame")
    assertClass(dtw.refs, "list")
    assertNumeric(unlist(dtw.refs))
    # Transform data to matrix for stats::fft
    data = as.matrix(data[, col, drop = FALSE])
    feats_dtw = apply(data, 1L, function(x) getDtwDist(x, dtw.refs, method.align))
    feats_dtw = t(feats_dtw)
    # Add more legible column names to the output
    df = as.data.frame(feats_dtw)
    colnames(df) = stri_paste("dtw", seq_len(ncol(df)), sep = ".")
    df
  }

  ps = makeParamSet(
    makeDiscreteLearnerParam(id = "method.align", default = dtw::asymmetric, values = c(dtw::asymmetric, dtw::symmetric1, dtw::symmetric2))
  )

  makeExtractFDAFeatMethod(
    learn = lrn,
    reextract = lrn,
    args = list(dtw.refs = dtw.refs, method.align = method.align),
    par.set = ps
  )
}

