#' @title Functional Linear Array Model.
#'
#' @description
#' The function creates function data features based on the spline fit design
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
#' @param bsignal.knots [\code{integer}]\cr
#'   number of knots for spline fit, the more knots, the more flexible the the
#'   spline is, the easier for overfitting.
#' @param bsignal.df [\code{integer}]\cr
#'   degree of piecewise regression function.
#' @return Returns an \code{data.frame} object containing the design
#'   matrix(converted to dataframe) according to the spline fit.
#' @export
getFDAFDboostFeatures = function(data, target, have.target = TRUE, include.target = FALSE, bsignal.knots =10L ,bsignal.df = 3L) {
  requirePackages("mboost", default.method = "load")
  requirePackages("FDboost", default.method = "load")
  assert(
    checkClass(data, "data.frame"),
    checkClass(data, "matrix")
  )
  assertCharacter(target)
  assertFlag(include.target)
  assertFlag(have.target)
  assertInteger(bsignal.knots)
  assertInteger(bsignal.df)

  cns = colnames(data)
  if (target %in% cns && have.target == TRUE) {
    y = data[, target]
    data[, target] = NULL
  }

  # transform dataframe into matrix
  if (inherits(data, "data.frame"))
    data = as.matrix(data)

  blrn = FDboost::bsignal(x = as.matrix(data), s = 1:ncol(data), knots = bsignal.knots , degree = bsignal.df)
  features_bsignal = mboost::extract(object = blrn, what = "design")  # get the design matrix from base learner
  d_fdboost = as.data.frame(features_bsignal)
  if (include.target)
    d_fdboost[, target] = y
  return(list(feat = d_fdboost, meta = NULL))
}
