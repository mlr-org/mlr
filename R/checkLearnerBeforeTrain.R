checkLearnerBeforeTrain = function(task, learner, weights) {

  getColNames = function(task, property) {
    .data = getTaskData(task, functionals.as = "matrix")
    has.it = vlapply(.data, function(x) any(property(x)))
    clipString(collapse(colnames(.data)[has.it], ", "), 50L)
  }

  td = getTaskDesc(task)

  if (td$type != learner$type) {
    stopf("Task '%s' is '%s', but learner '%s' is for '%s'!", td$id, td$type, learner$id, learner$type)
  }

  if (td$has.missings && !hasLearnerProperties(learner, "missings")) {
    wrong.cols = getColNames(task, is.na)
    stopf("Task '%s' has missing values in '%s', but learner '%s' does not support that!", td$id, wrong.cols, learner$id)
  }

  if (td$n.feat["numerics"] > 0L && !hasLearnerProperties(learner, "numerics")) {
    wrong.cols = getColNames(task, is.numeric)
    stopf("Task '%s' has numeric inputs in '%s', but learner '%s' does not support that!", td$id, wrong.cols, learner$id)
  }

  if (td$n.feat["factors"] > 0L && !hasLearnerProperties(learner, "factors")) {
    wrong.cols = getColNames(task, is.factor)
    stopf("Task '%s' has factor inputs in '%s', but learner '%s' does not support that!", td$id, wrong.cols, learner$id)
  }

  if (td$n.feat["ordered"] > 0L && !hasLearnerProperties(learner, "ordered")) {
    wrong.cols = getColNames(task, function(x) class(x)[1] == "ordered")
    stopf("Task '%s' has ordered factor inputs in '%s', but learner '%s' does not support that!", td$id, wrong.cols, learner$id)
  }

  if (td$n.feat["functionals"] > 1 && hasLearnerProperties(learner, "single.functional") &&
    !hasLearnerProperties(learner, "functionals")) {
    stopf("Task '%s' has more than one functional inputs,
      but learner '%s' does not support that!", td$id, learner$id)
  }

  if (!(missing(weights) || is.null(weights)) && !hasLearnerProperties(learner, "weights")) {
    stopf("Weights vector passed to train, but learner '%s' does not support that!", learner$id)
  }

  if (td$has.weights && !hasLearnerProperties(learner, "weights")) {
    warningf("Task '%s' contains weights but these are not used by learner '%s'!", td$id, learner$id)
  }

  if (td$type == "classif") {
    if (length(td$class.levels) == 1L) {
      if (!hasLearnerProperties(learner, "oneclass")) {
        stopf("Task '%s' is a one-class-problem, but learner '%s' does not support that!", td$id, learner$id)
      }
    } else if (length(td$class.levels) == 2L) {
      if (!hasLearnerProperties(learner, "twoclass")) {
        stopf("Task '%s' is a two-class-problem, but learner '%s' does not support that!", td$id, learner$id)
      }
    } else {
      if (!hasLearnerProperties(learner, "multiclass")) {
        stopf("Task '%s' is a multiclass-problem, but learner '%s' does not support that!", td$id, learner$id)
      }
    }
  }
  invisible(NULL)
}
