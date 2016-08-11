#' @title Extracts out of bag predictions from trained models
#'
#' @description
#' Learners like \code{randomForest} produce out of bag predictions. 
#' \code{getOOBPreds} extracts this information from trained models and builds a 
#' prediction object like provided by predict. 
#' See \sQuote{Details} for a list of learners for which this is implemented.
#'
#' The following learners support out of bag predictions:
#' \itemize{
#' \item{randomForest} \cr
#' {Support for classification and regression.}
#' \item{randomForestSRC} \cr
#' {Regression, classification as well as for survival.}
#' \item{ranger} \cr
#' {Support for classification and regression.}
#' \item{rFerns} \cr
#' {Only for classification.}
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
getOOBPreds = function(object, task) {
  assertClass(object, classes = "WrappedModel")
  assertClass(task, classes = "Task")
  checkModelCorrespondsTask(object, task)

  td = object$task.desc
  # extract truth column
  subset = object$subset
  data = getTaskData(task, subset)
  t.col = match(td$target, colnames(data)) 
  truth = data[, t.col, drop = TRUE]
  if (is.list(truth))
    truth = data.frame(truth)
  
  p = getOOBPredsLearner(object$learner, object)
  # time is set to NA, as "no" time is required for getting the out of bag predictions
  makePrediction(task.desc = td, row.names = rownames(data), id = subset, truth = truth,
                 predict.type = object$learner$predict.type, predict.threshold = object$learner$predict.threshold, y = p, time = NA)
}

getOOBPredsLearner = function(.learner, .model) {
  UseMethod("getOOBPredsLearner")
}
