#' @title Shapelet features transformation
#'
#' @description The function learns a shapelet model for classification. This
#'   includes k shapelet features with each length l for classification. Based
#'   on those shapelets, the raw time series curve data can be tranformed into
#'   shapelet-based feature transformation. Moreover, a classification is
#'   learned jointly. It is either a standard SVM or a
#'   logistic regression. Together with the shapelets, they provide the complete
#'   shapelet model for classification.
#'
#' @param curves [\code{data.frame},\code{matrix}]\cr
#'   Time series curve data.
#' @param label [\code{factor}]\cr
#'   Corresponding labels.
#' @param ... [\code{any}]\cr
#'   Addtional parameters passed to shapelet learning function. Please see
#'   \link{\code{shapeletLib}}.
#'
#' @return Returns an \code{list} object \code{model} containing the learned
#'   shapelet model. I.e.
#'   \itemize{
#'     \item shapelets [\code{matrix}]: \code{k} learned shapelets.
#'     \item w [\code{numeric}]: learned weights w.1,..., w.k.
#'     \item w.0 [\code{numeric}]: learned w.0.
#'     \item m [\code{matrix}]: shapelet-based data transformation .
#'     \item f.val [\code{numeric}]: learning loss in each iteration.
#'     \item method [\code{character}]: which loss was used.
#'     \item max.iter [\code{integer}]: maximal number of iterations.
#'     \item class [\code{character}]: binary or multi-class classification?
#'   }
#'
#' @export
getTSShapeletFeatures = function(curves, label, ...) {

  requirePackages("shapeletLib", default.method = "load")

  assert(
    checkClass(curves, "data.frame"),
    checkClass(curves, "matrix")
  )
  assertFactor(label)


  shapeletmodel = shapeletLib::learnShapeletModel(data = curves, label = label, ...)

  return(list(model = shapeletmodel))

}
