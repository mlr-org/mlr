#' @title Extract Functional Principal Component Analysis Features.
#'
#' @description
#' The function extracts the functional principal components from a data.frame
#' containing functional features
#'
#' @param data [\code{data.frame}]\cr
#'   Data.frame containing matricies with one row per observation of a single functional
#'   or time series and one column per measurement time point. All entries need to be numeric.
#' @param target [\code{character}]\cr
#'   Name of the target variable. Default: \dQuote{NULL}. The variable is only
#'   set to be consistent with the API.
#' @param cols [\code{character} | \code{numeric}]\cr
#'   Column names or indices, the extraction should be performed on.
#' @param pve [\code{numeric}]\cr
#'   Fraction of variance explained for the functional principal components.
#' @param npc [\code{integer}]\cr
#'   Number of principal components to extract. Overrides \code{pve} param.
#'
#' @return Returns a \code{data.frame}.
#' @export
extractFpcaFeatures = function(data, target, cols, pve = 0.99, npc = NULL) {
  requirePackages("mboost", default.method = "load")
  requirePackages("refund", default.method = "load")
  assert(
    checkClass(data, "data.frame"),
    checkClass(data, "matrix")
  )

  data = data[, cols, drop = FALSE]

  # transform dataframe into matrix
  if (inherits(data, "data.frame"))
    data = as.matrix(data)

  # extract fpca features
  # Fixme: Add other fpca. options, maybe via function args ?
  rst = refund::fpca.sc(Y = data, pve = pve, npc = npc)
  # Order the columns by score
  features.fpca = rst$scores[, order(rst$evalues,  decreasing = TRUE)]
  df.fpca = as.data.frame(features.fpca)
  names(df.fpca) = paste0("Fpca", seq_len(ncol(df.fpca)))
  return(d.fpca)
}
