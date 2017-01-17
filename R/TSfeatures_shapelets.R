#' @title Shapelet features transformation
#'
#' @description The function learns shapelet features for classification. Based
#'   on those shapelets (k x l), the raw time series curve data is tranformed.
#'
#' @param curves [\code{data.frame},\code{matrix}]\cr
#'   Time series curve data.
#' @param label.train [\code{factor}]\cr
#'   Corresponding labels of curves.
#' @param ... [\code{any}]\cr
#'   Addtional parameters passed to shapelet learning function.
#'
#' @return Returns an \code{list} object \code{model} containing the learned shapelet model.
#'   I.e.
#'   \itemize{
#'     \item shapelets [\code{matrix}]: \code{k} learned shapelets.
#'     \item w [\code{numeric}]: learned weights w_1,..., w_k.
#'     \item w.0 [\code{numeric}]: learned w_0.
#'     \item m [\code{matrix}]: shapelet-based data transformation .
#'     \item f.val [\code{numeric}]: learning loss for each iteration.
#'     \item method [\code{character}]: which loss was used.
#'     \item max.iter [\code{integer}]: maximal number of iterations.
#'     \item class [\code{character}]: binary or multi classification?
#'   }
#'
#' @export
getTSShapeletFeatures = function(curves, label.train, ...) {

  requirePackages("shapeletLib", default.method = "load")

  assert(
    checkClass(curves, "matrix"),
    checkClass(curves, "data.frame")
  )


  #if curves is a data.frame, transform to matrix s.t. shapelet learning executes correctly
  if(inherits(curves, "data.frame"))
    curves = as.matrix(curves)

  if(!is.factor(label.train))
    label.train = as.factor(label.train)


  shapeletmodel = shapeletLib::learnShapelets(data = curves, label = label.train, ...)

  return(list(model = shapeletmodel))

}
