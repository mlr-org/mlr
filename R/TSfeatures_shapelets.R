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
getTSShapeletFeatures = function(curves, label.train, class.type = "binary", method = "hinge", max.iter = 100, K = 0.2, L = 0.1, C = 1, step = 0.01, init.method = "kmeans", auto.hinge = FALSE) {

  requirePackages("shapeletLib", default.method = "load")

  assert(
    checkClass(curves, "matrix"),
    checkClass(curves, "data.frame"),
    checkClass(method, "character"),
    checkClass(max.iter, "numeric"),
    checkClass(K, "numeric"),
    checkClass(L, "numeric"),
    checkClass(C, "numeric"),
    checkClass(step, "numeric"),
    checkClass(step, "character"),
    checkClass(init.method, "character"),
    checkClass(auto.hinge, "logical")
    )

  #if curves is a data.frame, transform to matrix s.t. shapelet learning executes correctly
  if(inherits(curves, "data.frame"))
    curves = as.matrix(curves)

  if( !(all(c(-1,1) %in% unique(label.train)) ) )
    stop("The labels must only contain -1 and 1 values.")

  shapeletmodel = shapeletLib::shapelet_classification(method = method,
                                                         class.type = class.type, K = K * dim(curves)[2],
                                                         L = L * dim(curves)[2], C = C,
                                                         data.train = curves,
                                                         label.train =  label.train,
                                                         step = step, max.iter = max.iter,
                                                         init.method = init.method)

  return(list(model = shapeletmodel))

#   requirePackages("wavelets", default.method = "load")
#
#   assert(
#     checkClass(curves, "data.frame"),
#     checkClass(curves, "matrix")
#   )
#
#   if (is.null(filter)) filter = "la8"
#   if (is.null(boundary)) boundary = "periodic"
#
#   # if matrix, transform to data.frame so that following code executes correctly
#   if(inherits(curves, "matrix"))
#     curves = as.data.frame(curves)
#
#   wtdata = NULL
#   for (i in seq_row(curves)) {
#     a = t(curves[i,])
#     wt = wavelets::dwt(a, filter = filter, boundary = boundary)
#     wtdata = rbind(wtdata, unlist(c(wt@W,wt@V[[wt@level]])))
#   }
#   wtdata = as.data.frame(wtdata)
#   return(wtdata)

}
