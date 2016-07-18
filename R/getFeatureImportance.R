#' Calculates feature importance values for trained models.
#'
#' For some learners it is possible to calculate a feature importance measure.
#' \code{getFeatureImportance} extracts those values from trained models.
#' See Details for a list of the implemented learners that support this.
#' 
#' 
#' The following learners have been implemented with the associated importance measures.
#' 
#' \itemize{
#'    \item{boosting} \cr
#'    {Measure which accounts the gain of Gini index given by a feature 
#'    in a tree and the weight of that tree.}
#'    \item{cforest} \cr
#'    {Permutation principle of the 'mean decrease in accuracy' principle
#'    in randomForest. If \code{AUC=TRUE} (only for classification and regression),
#'    area under the curve is used as measure.  The algorithm used for the survival
#'    learner is 'extremely slow and experimental; use at your own risk'.
#'    See \link[party:varimp]{varimp} for details and further parameters.}  
#'    \item{gbm} \cr
#'    {Estimation of relative influence for each feature. See 
#'    \link[gbm:relative.influence]{relative.influence}
#'    for details and further parameters.}
#'    \item{randomForest} \cr
#'    {For \code{type = 2} (the default) the 'MeanDecreaseGini' is measured,
#'    which is based on the Gini impurity index used for the calculation of the nodes.
#'    Alternatively, you can set \code{type} to 1, then the measure is the mean
#'    decrease in accuracy calculated on OOB data. Note, that in this case
#'    the learner's parameter \code{importance} needs to be set to be able to compute
#'    feature importance values.
#'    See \link[randomForest:importance]{importance} for details.}
#'    \item{RRF} \cr
#'    {This is identical to randomForest.}
#'    \item{randomForestSRC} \cr
#'    {This method can calculate feature importance for
#'    various measures. By default the Breiman-Cutler permutation method is used.
#'    See \link[randomForestSRC:vimp]{vimp} for details.}
#'    \item{ranger} \cr
#'    {Supports both measures mentioned above for the randomForest
#'    learner. Note, that you need to specifically set the learners parameter
#'    \code{importance}, to be able to compute feature importance measures.
#'    See \link[ranger:importance]{importance} and 
#'    \link[ranger:ranger]{ranger} for details.}
#'    \item{rpart} \cr
#'    {Sum of decrease in impurity for each of the surrogate variables at each node.}
#'    \item{xgboost} \cr
#'    {The value implies the relative contribution of the corresponding feature to the model 
#'    calculated by taking each feature's contribution for each tree in the model. The exact 
#'    computation of the importance in xgboost is undocumented.}
#'  }
#'
#' @section Implementing new methods:
#' You can add more learners by implementing methods for them.
#' Your implementation must adhere to the following: \cr
#' The method must take arguments \code{.learner}, \code{.model} and \code{...}.
#' Where, the model was fitted on the subset of \code{.task} given by \code{.subset}. 
#' The parameters in \code{...} should be parameters of the underlying importance value
#' generating function, so they just need to be passed to it directly. 
#' 
#' @param object [\code{\link{WrappedModel}}]\cr
#'   Wrapped model, result of \code{\link{train}}.
#' @param ... [any]\cr
#'   Additional parameters, which are passed to the underlying importance value 
#'   generating function.
#' @return A named numeric vector indicating the feature importance values for each feature.
#' @export
getFeatureImportance = function(object, ...) {
  getFeatureImportanceLearner(object$learner, object, ...)
}

#' Calculates feature importance values for a given learner.
#'
#' For details see \code{\link{getFeatureImportance}}.
#'
#' @param .learner [\code{\link{Learner}} | \code{character(1)}]\cr
#'   The learner.
#'   If you pass a string the learner will be created via \code{\link{makeLearner}}.
#' @param .model [\code{\link{WrappedModel}}]\cr
#'  The model.
#' @param auc [logical(1)]\cr Only for cforest. Should the auc based importance be calculated.
#' @param n.trees [integer(1)]\cr Only for gbm. For how many trees should the importance be calculated.
#'  If missing the number of trees used to fit the model is used.
#' @param ... [any]\cr
#' Additional parameters, which are passed to the underlying importance value 
#' generating function.
#' @export
getFeatureImportanceLearner = function(.learner, .model, ...) {
	UseMethod("getFeatureImportance")
}
