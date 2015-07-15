#' Calculate the feature importance commonly used by learner.
#'
#' Mainly for internal use.
#' You have to implement this method if you want to add another learner to this package.
#'
#' You implementation must adhere to the following:
#' The model must be fitted on the subset of \code{.task} given by \code{.subset}. 
#' All parameters in \code{...} will be passed to the underlying importance value
#' generating function.
#' 
#' The following learners have been implemented with the associated importance measures:
#' 
#' \describe{
#'    \item{boosting}{Measure which accounts the gain of Gini index given by a feature 
#'    in a tree and the weight of that tree.}
#'    \item{cforest}{Permutation principle of the 'mean decrease in accuracy' principle
#'    in randomForest. If \code{AUC=TRUE}, area under the curve is used as measure. See
#'    party::varimp for details and further parameters.}  
#'    \item{gbm}{Estimation of relative influence for each feature. See 
#'    gbm::relative.influence for details and further parameters.}
#'    \item{randomForest}{Per default, the MeanDecreaseGini is measured, which is based 
#'    on the Gini impurity index used for the calculation of the nodes.}
#'    \item{rpart}{Sum of decrease in impurity for each of the surrogate variables at 
#'    each node.}
#'  }
#' 
#' @param model [\code{\link{WrappedModel}}]\cr
#'   Model produced by training. The learner in use can be deduced from this.
#' @param ... [any]\cr
#'   Additional parameters, which are passed to the underlying importance value 
#'   generating function.
#' @return A numeric vector indicating the feature importance values for each feature.
#' @export
getFeatureImportance = function(model, ...) {
    getFeatureImportanceS3(model$learner, model, ...)
}

getFeatureImportanceS3 = function(.learner, .model, ...) {
    UseMethod("getFeatureImportance")
}