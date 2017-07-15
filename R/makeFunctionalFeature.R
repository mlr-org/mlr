#' Create a functional feature from a numeric matrix.
#'
#' @param mat [\code{matrix|data.frame}] \cr
#'   Numeric matrix or data.frame that contains the functional features.
#' @return [\code{matrix}] \cr
#'   Matrix containing the functional values.
#' @export
makeFunctionalFeature = function(mat) {
  if (is.data.frame(mat))
    mat = as.matrix(mat)
  if (any(dim(mat) %in% 0))
    stop("Matrix dimensions need to be > 0.")
  assertMatrix(mat, mode = "numeric")
}
