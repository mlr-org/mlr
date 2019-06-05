#' @title Extracts out-of-bag predictions from trained models.
#'
#' @description
#' Learners like `randomForest` produce out-of-bag predictions.
#' `getOOBPreds` extracts this information from trained models and builds a
#' prediction object as provided by predict (with prediction time set to NA).
#' In the classification case:
#' What is stored exactly in the ([Prediction]) object depends
#' on the `predict.type` setting of the [Learner].
#'
#' You can call `listLearners(properties = "oobpreds")` to get a list of learners
#' which provide this.
#'
#' @template arg_wrappedmod
#' @template arg_task
#' @return ([Prediction]).
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
  checkTask(task, task.type = c("classif", "regr", "surv"))
  checkModelCorrespondsTask(model, task)

  td = model$task.desc
  # extract truth column
  subset = model$subset
  data = getTaskData(task, subset)
  truth = data[, td$target]

  p = getOOBPredsLearner(model$learner, model)
  # time is set to NA, as "no" time is required for getting the out of bag predictions
  checkPredictLearnerOutput(model$learner, model, p)
  makePrediction(task.desc = td, row.names = rownames(data), id = subset, truth = truth,
    predict.type = model$learner$predict.type, predict.threshold = model$learner$predict.threshold, y = p, time = NA)
}

#' @title Provides out-of-bag predictions for a given model and the corresponding learner.
#'
#' @description
#' This function is mostly for internal usage. To get out-of-bag predictions use [getOOBPreds].
#'
#' @param .learner ([Learner])\cr
#'   The learner.
#' @param .model ([WrappedModel])\cr
#'   Wrapped model.
#' @return Same output structure as in ([predictLearner]).
#' @export
#' @keywords internal
getOOBPredsLearner = function(.learner, .model) {
  UseMethod("getOOBPredsLearner")
}

#' @export
getOOBPredsLearner.BaseWrapper = function(.learner, .model) {
  getOOBPredsLearner(.learner$next.learner, .model = .model)
}

# checks if the model was trained on the corresponding task by comparing
# the descriptions
checkModelCorrespondsTask = function(model, task) {
  compare = c("id", "type", "target", "n.feats", "has.weights", "has.blocking", "is.spatial", "positive")
  if (!identical(task$task.desc[compare], model$task.desc[compare])) {
    stopf("Description of the model does not correspond to the task")
  }
}
