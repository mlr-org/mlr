#' @title Extract functional principal component analysis features.
#'
#' @description
#' The function extracts the functional principal components from a data.frame
#' containing functional features.
#'
#' @param pve (`numeric(1)`)\cr
#'   Fraction of variance explained for the functional principal components.
#'   Default is 0.99.
#' @param npc (`integer(1)`)\cr
#'   Number of principal components to extract. Overrides `pve` param.
#'   Default is `NULL`
#' @return ([data.frame]).
#' @export
#' @family fda_featextractor
extractFDAFPCA = function(pve = 0.99, npc = NULL) {
  assertNumber(pve, lower = 0, upper = 1)
  assertCount(npc, null.ok = TRUE)

  lrn = function(data, target, col, vals, pve = 0.99, npc = NULL) {
    requirePackages("mboost", default.method = "load")
    requirePackages("refund", default.method = "load")
    assert(
      checkClass(data, "data.frame"),
      checkClass(data, "matrix")
    )

    data = data[, col, drop = FALSE]

    # transform dataframe into matrix
    if (inherits(data, "data.frame"))
      data = as.matrix(data)

    # extract fpca features
    # FIXME: Add other fpca. options, maybe via function args ?
    rst = refund::fpca.sc(Y = data, pve = pve, npc = npc)
    # Order the columns by score
    features.fpca = rst$scores[, order(rst$evalues,  decreasing = TRUE)]
    df.fpca = as.data.frame(features.fpca)
    names(df.fpca) = paste0("Fpca", seq_len(ncol(df.fpca)))
    return(df.fpca)
  }
  ps = makeParamSet(
    makeNumericParam("pve", lower = 0, upper = 1),
    makeIntegerParam("npc", lower = 1, upper = Inf)
  )
  makeExtractFDAFeatMethod(learn = lrn, reextract = lrn, args = list(pve = pve, npc = npc), par.set = ps)
}


#' @title Bspline mlq features
#'
#' @description
#' The function extracts features from functional data based on the Bspline fit.
#' For more details refer to \code{\link[FDboost]{bsignal}}.
#'
#' @param bsignal.knots (`integer(1)`)\cr
#'   The number of knots for bspline.
#' @param bsignal.df (`numeric(1)`)\cr
#'   The effective degree of freedom of penalized bspline.
#' @return ([data.frame]).
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
    feats.bsignal = mboost::extract(object = blrn, what = "design")  # get the design matrix of the base learner
    # Add more legible column names to the output
    df = as.data.frame(feats.bsignal)
    colnames(df) = stri_paste("bsig", seq_len(ncol(df)), sep = ".")
    df
  }
  ps = makeParamSet(
    makeIntegerParam("bsignal.knots", lower = 3L, upper = Inf, default = 10L),
    makeNumericParam("bsignal.df", lower = 0.9, upper = Inf, default = 3)
  )
  makeExtractFDAFeatMethod(
    learn = lrn,
    reextract = lrn,
    args = list(bsignal.knots = bsignal.knots, bsignal.df = bsignal.df),
    par.set = ps
  )
}

