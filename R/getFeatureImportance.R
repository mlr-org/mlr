#' @title Calculates feature importance values for trained models.
#'
#' @description
#' For some learners it is possible to calculate a feature importance measure.
#' `getFeatureImportance` extracts those values from trained models.
#' See below for a list of supported learners.
#'
#'
#' \itemize{
#'    \item{boosting} \cr
#'    {Measure which accounts the gain of Gini index given by a feature
#'    in a tree and the weight of that tree.}
#'    \item{cforest} \cr
#'    {Permutation principle of the 'mean decrease in accuracy' principle
#'    in randomForest. If `auc=TRUE` (only for binary classification),
#'    area under the curve is used as measure.  The algorithm used for the survival
#'    learner is 'extremely slow and experimental; use at your own risk'.
#'    See \link[party:varimp]{varimp} for details and further parameters.}
#'    \item{gbm} \cr
#'    {Estimation of relative influence for each feature. See
#'    \link[gbm:relative.influence]{relative.influence}
#'    for details and further parameters.}
#'    \item{randomForest} \cr
#'    {For `type = 2` (the default) the 'MeanDecreaseGini' is measured,
#'    which is based on the Gini impurity index used for the calculation of the nodes.
#'    Alternatively, you can set `type` to 1, then the measure is the mean
#'    decrease in accuracy calculated on OOB data. Note, that in this case
#'    the learner's parameter `importance` needs to be set to be able to compute
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
#'    `importance`, to be able to compute feature importance measures.
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
#' @param object ([WrappedModel])\cr
#'   Wrapped model, result of [train].
#' @param ... (any)\cr
#'   Additional parameters, which are passed to the underlying importance value
#'   generating function.
#' @return ([FeatureImportance]) An object containing a `data.frame` of the variable importances and further information.
#' @noMd
#' @export
getFeatureImportance = function(object, ...) {

  assertClass(object, classes = "WrappedModel")
  lrn = checkLearner(object$learner, props = "featimp")
  imp = getFeatureImportanceLearner(lrn, object, ...)

  if (!check_numeric(imp, names = "unique") && !check_subset(names(imp), object$features))
    stop("getFeatureImportanceLearner did not return a named vector with names of the task features.")

  #We need to add missing pars with zero and order them
  imp[setdiff(object$features, names(imp))] = 0
  imp = imp[object$features]

  #convert named vector to data.frame with columns and set NA to 0
  imp[is.na(imp)] = 0L
  imp = as.data.frame(t(imp))
  rownames(imp) = NULL

  makeS3Obj("FeatureImportance",
    res = imp,
    task.desc = getTaskDesc(object),
    learner = lrn,
    measure = NA,
    contrast = NA,
    aggregation = identity,
    nmc = NA,
    replace = NA,
    local = FALSE)
}

#' @title Calculates feature importance values for a given learner.
#'
#' @description
#'
#' This function is mostly for internal usage. To calculate feature importance use [getFeatureImportance].
#'
#' The return value is a named numeric vector. There does not need to be one value for each feature in the dataset.
#' In [getFeatureImportance] missing features will get an importance of zero and if the vector contains `NA`
#' they will also be replaced with zero.
#'
#' @param .learner ([Learner] | `character(1)`)\cr
#'   The learner.
#' @param .model ([WrappedModel])\cr
#'  The model.
#' @param ... (any)\cr
#' Additional parameters, which are passed to the underlying importance value
#' generating function.
#' @return ([numeric]) A named vector of variable importance.
#' @export
#' @keywords internal
getFeatureImportanceLearner = function(.learner, .model, ...) {
  UseMethod("getFeatureImportanceLearner")
}

#' @export
getFeatureImportanceLearner.BaseWrapper = function(.learner, .model, ...) {
  getFeatureImportanceLearner(.learner$next.learner, .model = .model, ...)
}
