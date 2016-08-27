#' @title Extracts out of bag predictions from trained models.
#'
#' @description
#' Learners like \code{randomForest} produce out of bag predictions. 
#' \code{getOOBPreds} extracts this information from trained models and builds a 
#' prediction object like provided by predict (with prediction time set to NA). 
#' In the classification case: 
#' What is stored exactly in the [\code{\link{Prediction}}] object depends
#' on the \code{predict.type} setting of the \code{\link{Learner}}.
#' If \code{predict.type} was set to \dQuote{prob} probability thresholding
#' can be done calling the \code{\link{setThreshold}} function on the
#' prediction object.
#' See \sQuote{Details} for a list of learners for which this is implemented. 
#'
#' The following learners support out of bag predictions:
#' \itemize{
#' \item{randomForest} \cr
#' {Support for classification and regression.}
#' \item{randomForestSRC} \cr
#' {Support for classification, regression and survival.}
#' \item{ranger} \cr
#' {Support for classification and regression.}
#' \item{rFerns} \cr
#' {Support for classification.}
#' }
#'
#' @template arg_wrappedmod
#' @template arg_task
#' @return [\code{\link{Prediction}}].
#' @export
#' @examples
#' training.set = sample(1:150, 50)
#' lrn = makeLearner("classif.ranger", predict.type = "prob", predict.threshold = 0.6)
#' mod = train(lrn, sonar.task, subset = training.set)
#' oob = getOOBPreds(mod, sonar.task)
#' oob
#' performance(oob, measures = list(auc, mmce))
getOOBPreds = function(model, task) {
  assertClass(model, classes = "WrappedModel")
  assertClass(task, classes = "Task")
  checkModelCorrespondsTask(model, task)

  td = model$task.desc
  # extract truth column
  subset = model$subset
  data = getTaskData(task, subset)
  t.col = match(td$target, colnames(data)) 
  truth = data[, t.col, drop = TRUE]
  if (is.list(truth))
    truth = data.frame(truth)
  
  p = getOOBPredsLearner(model$learner, model)
  # time is set to NA, as "no" time is required for getting the out of bag predictions
  checkPredictLearnerOutput(model$learner, model, p)
  makePrediction(task.desc = td, row.names = rownames(data), id = subset, truth = truth,
    predict.type = model$learner$predict.type, predict.threshold = model$learner$predict.threshold, y = p, time = NA)
}

#' @title Provides out of bag predictions for a given model and the corresponding learner.
#' 
#' @description 
#' 
#' This function is mostly for internal usage. To get out-of-bag predictions use \code{\link{getOOBPreds}}.
#' 
#' @param .learner [\code{\link{Learner}} | \code{character(1)}]\cr
#'   The learner.
#' @param .model [\code{\link{WrappedModel}}]\cr
#'   Wrapped model, result of \code{\link{train}}, has to correspond to the learner.
#' @return [\code{matrix} | \code{factor} | \code{numeric}]\cr
#' The return value depends on the learner. If the learner is a classification learner and 
#' prediction type is probability, the outcome is a numeric matrix, each column corresponding to a 
#' level. If prediction type is response, it is a factor vector with the levels of the target variable.
#' If the learner is a regression learner the outcome is a numeric vector. 
#' @export
#' @keywords internal
getOOBPredsLearner = function(.learner, .model) {
  UseMethod("getOOBPredsLearner")
}
