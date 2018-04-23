#' @title DTW kernel features
#'
#' @description
#' The function extracts features from functional data based on the DTW distance with a reference dataframe.
#'
#' @param ref.method (`character(1)`)\cr
#'   How should the reference curves be obtained?
#'   Method `random` draws `n.refs` random reference curves, while `all` uses all curves as references.
#'   In order to use user-provided reference curves, this parameter is set to `fixed`.
#' @param n.refs (`numeric(1)`)\cr
#'   Number of reference curves to be drawn (as a fraction of the number of observations in the training data).
#' @param refs (`matrix`|`integer(n)`)\cr
#'   Integer vector of training set row indices or a matrix of reference curves with the same length as
#'   the functionals in the training data. Overwrites `ref.method` and `n.refs`.
#' @param dtwwindow (`numeric(1)`)\cr
#'   Size of the warping window size (as a proportion of query length).
#' @return ([data.frame]).
#' @export
#' @family fda_featextractor
extractFDADTWKernel = function(ref.method = "random", n.refs = 0.05, refs = NULL, dtwwindow = 0.05) {
  requirePackages("rucrdtw", default.method = "attach")

  # Function that extracts dtw-distances for a single observation and a set of reference
  # curves
  getDtwDist = function(frow, refs, dtwwindow) {
    # Compute dtw distance from the selected row to each reference row
    row = vnapply(seq_len(nrow(refs)), function(i) rucrdtw::ucrdtw_vv(frow, refs[i, ], dtwwindow)$distance)
    return(row)
  }
  lrn = function(data, target = NULL, col, ref.method = "random", n.refs = 0.05, refs = NULL, dtwwindow = 0.05) {
    assertClass(data, "data.frame")
    assertChoice(ref.method, c("random", "all", "fixed"))
    assertNumeric(n.refs, lower = 0, upper = 1)
    assertChoice(class(refs), c("matrix", "integer", "NULL"))
    assertNumber(dtwwindow)

    data = as.matrix(data[col])

    # Obtain reference curves indices
    if (is.null(refs) | is.integer(refs)) {
      if (ref.method == "random")
        refs = sample(seq_len(nrow(data)), size = max(min(nrow(data), round(n.refs * nrow(data), 0)), 2L))
      if (ref.method == "all")
        refs = seq_len(nrow(data))
      refs.data = data[refs, , drop = FALSE]
    } else {
      refs.data = refs
    }

    feats.dtw = t(apply(data, 1L, function(x) getDtwDist(x, refs.data, dtwwindow)))

    # Add more legible column names to the output
    df = as.data.frame(feats.dtw)
    colnames(df) = stri_paste("dtw", seq_len(ncol(df)), sep = ".")
    return(df)
  }

  ps = makeParamSet(
    makeDiscreteParam(id = "ref.method", default = "random", values = c("random", "all", "fixed")),
    makeNumericParam(id = "n.refs", default = 0.05, lower = 0, upper = 1),
    makeUntypedParam(id = "refs", default = NULL),
    makeNumericParam(id = "dtwwindow", lower = 0, upper = 1)
  )

  makeExtractFDAFeatMethod(
    learn = lrn,
    reextract = lrn,
    args = list(ref.method = ref.method, n.refs = n.refs, refs = refs, dtwwindow = dtwwindow),
    par.set = ps
  )
}

