#' Extracts out of bag data from trained models
#'
#' Learners like \code{randomForest} produce out of bag predictions and error
#' rates. \code{getOutOfBag} extracts this information from trained models.
#' See \sQuote{Details} for a list of learners for which this is implemented.
#'
#'
#' The way how the out of bag error is computed depends on the underlying task.
#' For classification it is the \sQuote{mean misclassification error}
#' on oob data. For regression and survival tasks
#' it is the mean of the residuals.
#'
#' The following learners support out of bag predictions:
#' \itemize{
#' \item{randomForest} \cr
#' {Support for classification and regression.}
#' \item{randomForestSRC} \cr
#' {Regression, classification as well as a survival method exists.}
#' \item{ranger} \cr
#' {Full support for classification and regression. For survival
#' only the overall out of bag error is available.}
#' \item{rFerns} \cr
#' {Only for classification.}
#' }
#'
#' @param object [\code{\link{WrappedModel}}]\cr
#'   Wrapped model, result of \code{\link{train}}.
#' @return A list with slots \code{response}, containing the out of bag
#' predictions, and \code{err} with the error rate.
#' @export
getOutOfBag = function(object) {
  getOutOfBagS3(object$learner, object)
}

getOutOfBagS3 = function(.learner, .model) {
  UseMethod("getOutOfBag")
}
