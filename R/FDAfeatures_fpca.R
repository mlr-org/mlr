#' @title Functional Linear Array Model.
#'
#' @description
#' The function creates functional data features based on the spline fit design
#' matrix.
#'
#' @param data [\code{data.frame},\code{matrix}]\cr
#'   Functional data.
#' @param target [\code{character}]\cr
#'   Name of the target variable.
#' @param have.target [\code{logical}]\cr
#'   Does the input has the target column?
#' @param include.target [\code{logical}]\cr
#'   Should the target variable (i.e. the label) be added in the returned
#'   data.frame? Default is \code{FALSE}.
#' @return Returns a \code{data.frame}.
#' @export
getFDAFPCAFeatures = function(data, target, have.target = TRUE, include.target = FALSE) {
  requirePackages("mboost", default.method = "load")
  requirePackages("refund", default.method = "load")
  assert(
    checkClass(data, "data.frame"),
    checkClass(data, "matrix")
  )
  assertCharacter(target)
  assertFlag(include.target)
  assertFlag(have.target)

  cns = colnames(data)
  if (target %in% cns && have.target == TRUE) {
    y = data[, target]
    data[, target] = NULL
  }

  # transform dataframe into matrix
  if (inherits(data, "data.frame"))
    data = as.matrix(data)

  rst = refund::fpca.sc(Y = data)
  features_fpca = rst$scores
  d_fpca = as.data.frame(features_fpca)
  names(d_fpca) = paste0("V",1:ncol(d_fpca))
  if (include.target)
    d_fpca[, target] = y
  return(list(feat = d_fpca, meta = NULL))
}
