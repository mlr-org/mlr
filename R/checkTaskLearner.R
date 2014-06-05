checkTaskLearner = function(task, learner, weights) {
  getColNames =  function(task, property){
    .data = getTaskData(task)
    has.it = apply(.data, 2, function(x) any(property(x)))
    clipString(collapse(colnames(.data)[has.it]), 50L)
  }

  td = task$task.desc
  if (td$type != learner$type)
    stopf("Task '%s' is '%s', but learner '%s' is for '%s'!", td$id, td$type, learner$id, learner$type)
  if (td$has.missings && !learner$missings) {
    wrong.cols = getColNames(task, is.na)
    stopf("Task %s has missing values in %s, but learner '%s' does not support that!", td$id, wrong.cols, learner$id)
  }
  if (td$n.feat["numerics"] > 0L && !learner$numerics) {
    wrong.cols = getColNames(task, is.numeric)
    stopf("Task %s has numeric inputs in %s, but learner '%s' does not support that!", td$id, wrong.cols, learner$id)
  }
  if (td$n.feat["factors"] > 0L && !learner$factors) {
    wrong.cols = getColNames(task, is.factor)
    stopf("Data set has factor inputs in %s, but learner '%s' does not support that!", td$id, wrong.cols, learner$id)
  }
  if (td$type == "classif" && length(td$class.levels)> 2L && !learner$multiclass)
    stopf("Task %s is a multiclass-problem, but learner '%s' does not support that!", td$id, learner$id)
  if (!(missing(weights) || is.null(weights)) && !learner$weights)
    stopf("Weights vector passed to train, but learner '%s' does not support that!", learner$id)
  if (td$has.weights && !learner$weights)
    warning("Task contains weights but these are not used by learner '%s'!", learner$id)
}

