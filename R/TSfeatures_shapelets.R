#' @title Shapelet features transformation
#'
#' @description The function learns shapelet features transformation on the raw time series curve data for classification.
#'
#' @param curves [\code{data.frame},\code{matrix}]\cr Time series curve data.
#' @param filter, periodic \cr Optional. Which filter should be used. Default:
#'   \code{filter} = \dQuote{la8}, \code{boundary} = \dQuote{periodic}. See
#'   package \code{wavelets} for more information.
#' @return Returns an \code{data.frame} object containing the shapelet feature transformation.
#'
#' @export
getTSShapeletFeatures = function(curves, label.train, ...) {

  requirePackages("shapeletLib", default.method = "load")

  # assert(
  #   checkClass(curves, "matrix"),
  #   checkClass(curves, "data.frame"),
  #   checkClass(method, "character"),
  #   checkClass(max.iter, "numeric"),
  #   checkClass(K, "numeric"),
  #   checkClass(L, "numeric"),
  #   checkClass(C, "numeric"),
  #   checkClass(step, "numeric"),
  #   checkClass(step, "character"),
  #   checkClass(init.method, "character"),
  #   checkClass(auto.hinge, "logical")
  #   )

  #if curves is a data.frame, transform to matrix s.t. shapelet learning executes correctly
  if(inherits(curves, "data.frame"))
    curves = as.matrix(curves)

  if( !(all(c(-1,1) %in% unique(label.train)) ) )
    stop("The labels must only contain -1 and 1 values.")

  shapeletmodel = shapeletLib::learnShapelets(data.train = curves, label.train = label.train, ...)

  return(list(model = shapeletmodel))



}
