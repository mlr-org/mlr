#' @title Confusion matrix.
#'
#' @description
#'
#' `getConfMatrix` is deprecated. Please use [calculateConfusionMatrix].
#'
#'
#' Calculates confusion matrix for (possibly resampled) prediction.
#' Rows indicate true classes, columns predicted classes.
#'
#' The marginal elements count the number of classification
#' errors for the respective row or column, i.e., the number of errors
#' when you condition on the corresponding true (rows) or predicted
#' (columns) class. The last element in the margin diagonal
#' displays the total amount of errors.
#'
#' Note that for resampling no further aggregation is currently performed.
#' All predictions on all test sets are joined to a vector yhat, as are all labels
#' joined to a vector y. Then yhat is simply tabulated vs y, as if both were computed on
#' a single test set. This probably mainly makes sense when cross-validation is used for resampling.
#'
#' @template arg_pred
#' @param relative (`logical(1)`)\cr
#'   If `TRUE` rows are normalized to show relative frequencies.
#'   Default is `FALSE`.
#' @return ([matrix]). A confusion matrix.
#' @export
#' @seealso [predict.WrappedModel]
getConfMatrix = function(pred, relative = FALSE) {
  .Deprecated("calculateConfusionMatrix")
  calculateConfusionMatrix(pred, relative = relative)
}
