#' @title Bspline mlq features
#'
#' @description
#' The function extracts features from functional data based on the Bspline fit.
#' For more details refer to \code{\link[FDboost]{bsignal}}.
#'
#' @param bsignal.knots [\code{integer}]\cr
#' The number of knots for bspline.
#' @param bsignal.df [\code{numeric}]\cr
#' The effective degree of freedom of penalized bspline.
#' @return [\code{data.frame}].
#' @export
#' @family fda_featextractor
extractFDABsignal = function(bsignal.knots = 10L, bsignal.df = 3) {
  lrn = function(data, target = NULL, col, bsignal.knots, bsignal.df) {
    assertClass(data, "data.frame")
    assertInteger(bsignal.knots)
    assertNumeric(bsignal.df)
    # Transform data to matrix for stats::fft
    data = as.matrix(data[, col, drop = FALSE])
    assertNumeric(data)
    blrn = FDboost::bsignal(x = data, s = 1:ncol(data), knots = bsignal.knots, degree = bsignal.df)
    feats_bsignal = mboost::extract(object = blrn, what = "design")  # get the design matrix of the base learner
    # Add more legible column names to the output
    df = as.data.frame(feats_bsignal)
    colnames(df) = stri_paste("bsig", seq_len(ncol(df)), sep = ".")
    df
  }
  makeExtractFDAFeatMethod(
    learn = lrn,
    reextract = lrn,
    args = list(bsignal.knots = bsignal.knots, bsignal.df = bsignal.df)
  )
}

