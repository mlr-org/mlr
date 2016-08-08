#' Extracts out of bag predictions from trained models
#'
#' Learners like \code{randomForest} produce out of bag predictions. 
#' \code{getOutOfBag} extracts this information from trained models and builds a 
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
#' @param object [\code{\link{WrappedModel}}]\cr
#'   Wrapped model, result of \code{\link{train}}.
#' @param task [\code{\link{Task}}]\cr
#'   The task. If this is passed, data from this task is predicted.
#' @return [\code{\link{Prediction}}].
#' @export
#' @examples
#' set.seed(123)
#' training.set = sample(1:150, 50)
#' lrn = makeLearner("classif.ranger", predict.type = "prob", predict.threshold = 0.6)
#' mod = train(lrn, sonar.task, subset = training.set)
#' oob = getOutOfBag(mod, sonar.task)
#' oob
#' performance(oob, measures = list(auc, mmce))
getOutOfBagPredictions = function(object, task) {
  assertClass(object, classes = "WrappedModel")
  assertClass(task, classes = "Task")
  td = object$task.desc
  # extract truth column
  subset = object$subset
  data = getTaskData(task, subset)
  t.col = match(td$target, colnames(data))
  if (!all(is.na(t.col))) {
    truth = data[, t.col, drop = TRUE]
    if (is.list(truth))
      truth = data.frame(truth)
  } else {
    truth = NULL
  }
  
  st = system.time(p <-  getOutOfBagPredictionsModel(object$learner, object))
  time.predict = as.numeric(st[3L])
  
  makePrediction(task.desc = td, row.names = rownames(data), id = subset, truth = truth,
                 predict.type = object$learner$predict.type, predict.threshold = object$learner$predict.threshold, y = p, time = time.predict)
}

getOutOfBagPredictionsModel = function(.learner, .model) {
  UseMethod("getOutOfBagPredictions")
}