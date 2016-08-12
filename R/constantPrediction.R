#' @title Make a constant prediction 
#'
#' @description 
#' This function is used to make the optimal constant prediction for a given task based on a measure.
#' 
#' In case of a regression task the measure is optimized with a line search between the min and max
#' value of the response. For classification the measure is evaluated for every class
#' of the response. If \code{predict.type = "prob"} the results over
#' all classes are scaled to 1 and the probability for every class is returned, if \code{predict.type = "response"}
#' only the best class label is returned.
#' 
#' Mostly for internal use. Featureleass learners that use this function can be created with 
#' \code{makeLearner("classif.featurelesss")} or \code{makeLearner("regr.featureless")}.
#'
#' @template arg_task
#' @template arg_measure
#' @param predict.type [\code{character(1)}]\cr
#'  Only meaningful for classification tasks. Either \code{predict.type = "response"} (the default)
#'  if the label of the best class is returned, or \code{predict.type = "prob"} for a vector of
#'  probabilities. 
#' @return [\code{factor(1)} | \code{numeric(1)} | \code{numeric}]\cr
#'  For regression task the best constant numeric prediction is returned. For classification see \code{predict.type}.
#' @export
constantPrediction = function(task, measure, predict.type = "response") {
  
  assert_choice(predict.type, c("response", "prob"))
  checkTask(task)
  measure = checkMeasures(measure)
  
  UseMethod("constantPrediction")
}

#' @export
constantPrediction.ClassifTask = function(task, measure, predict.type) {
  
  if (task$type %nin% measure$properties)
    stopf("Measure %s does not support task type %s!", measure$id, task$type)
  if ("req.model" %in% measure$properties)
    stopf("Measure %s requires a fitted model and cannot be used to define a learner!", measure$id)
  
  lvls = getTaskClassLevels(task)
  
  if (length(lvls) > 2L && "classif.multi" %nin% measure$properties)
    stopf("Multiclass problems cannot be used for measure %s!", measure$id)
  
  y = getTaskTargets(task)
  n = length(y)
  scores = vnapply(lvls, function(a) {
    arep = factor(rep(a, n), levels = lvls)
    data = data.frame(truth = y, response = arep)
    desc = makeS3Obj("TaskDesc", class.levels = lvls)
    p = makeS3Obj("Prediction", data = data, task.desc = desc)
    measure$fun(pred = p, extra.args = measure$extra.args)
  })
  
  scores = scores / sum(scores)

  if (predict.type == "prob")
    scores
  else if (measure$minimize)
    factor(names(scores)[which.min(scores)], levels = lvls)
  else
    factor(names(scores)[which.max(scores)], levels = lvls)
}

#' @export
constantPrediction.RegrTask = function(task, measure, ...) {
  
  if (task$type %nin% measure$properties)
    stopf("Measure %s does not support task type %s!", measure$id, task$type)
  if ("req.model" %in% measure$properties)
    stopf("Measure %s requires a fitted model and cannot be used to define a learner!", measure$id)
  
  
  y = getTaskTargets(task)
  n = length(y)
  f = function (a) {
    arep = rep(a, n)
    data = data.frame(truth = y, response = arep)
    desc = makeS3Obj("TaskDesc")
    p = makeS3Obj("Prediction", data = data, task.desc = desc)
    measure$fun(pred = p, extra.args = measure$extra.args)
  }
  res = optimize(f, c(min(y), max(y)), tol = 0.0001, maximum = !measure$minimize)
  res[[1]]
}