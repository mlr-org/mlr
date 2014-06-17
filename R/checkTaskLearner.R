checkTaskLearner = function(task, learner, weights) {
  getColNames =  function(task, property){
    .data = getTaskData(task)
    has.it = sapply(.data, function(x) any(property(x)))
    clipString(collapse(colnames(.data)[has.it], ", "), 50L)
  }
  td = task$task.desc
  if (td$type != learner$type)
    stopf("Task '%s' is '%s', but learner '%s' is for '%s'!", td$id, td$type, learner$id, learner$type)
  if (td$has.missings && !hasProperty(learner, "missings")) {
    wrong.cols = getColNames(task, is.na)
    stopf("Task '%s' has missing values in '%s', but learner '%s' does not support that!", td$id, wrong.cols, learner$id)
  }
  if (td$n.feat["numerics"] > 0L && !hasProperty(learner, "numerics")) {
    wrong.cols = getColNames(task, is.numeric)
    stopf("Task '%s' has numeric inputs in '%s', but learner '%s' does not support that!", td$id, wrong.cols, learner$id)
  }
  if (td$n.feat["factors"] > 0L && !hasProperty(learner, "factors")) {
    wrong.cols = getColNames(task, is.factor)
    stopf("Task '%s' has factor inputs in '%s', but learner '%s' does not support that!", td$id, wrong.cols, learner$id)
  }
  if (td$type == "classif") {
    if (length(td$class.levels) == 1L) {
      if(!hasProperty(learner, "oneclass"))
        stopf("Task '%s' is a one-class-problem, but learner '%s' does not support that!", td$id, learner$id)
    } else if (length(td$class.levels) == 2L) {
      if (!hasProperty(learner, "twoclass"))
        stopf("Task '%s' is a two-class-problem, but learner '%s' does not support that!", td$id, learner$id)
    } else {
      if (!hasProperty(learner, "multiclass"))
        stopf("Task '%s' is a multiclass-problem, but learner '%s' does not support that!", td$id, learner$id)
    }
  }
  if (!(missing(weights) || is.null(weights)) && !hasProperty(learner, "weights"))
    stopf("Weights vector passed to train, but learner '%s' does not support that!", learner$id)
  if (td$has.weights && !hasProperty(learner, "weights"))
    warning("Task '%s' contains weights but these are not used by learner '%s'!", td$id, learner$id)

  invisible(NULL)
}
