checkTaskLearner = function(task, learner, weights) {
  td = task$task.desc
  if (td$type != learner$type)
    stopf("Task %s is %s, but learner %s is for %s!", td$id, td$type, learner$id, learner$type)
  if (td$has.missings && !learner$missings)
    stopf("Task %s has missing values, but learner %s does not support that!", td$id, learner$id)
  if (td$n.feat["numerics"] > 0L && !learner$numerics)
    stopf("Task %s has numeric inputs, but learner %s does not support that!", td$id, learner$id)
  if (td$n.feat["factors"] > 0L && !learner$factors)
    stopf("Data set has factor inputs, but learner %s does not support that!", td$id, learner$id)
  if (td$type == "classif" && length(td$class.levels)> 2L && !learner$multiclass)
    stopf("Task %s is a multiclass-problem, but learner %s does not support that!", td$id, learner$id)
  if (!(missing(weights) || is.null(weights)) && !learner$weights)
    stopf("Weights vector passed to train, but learner %s does not support that!", learner$id)
  if (td$has.weights && !learner$weights)
    warning("Task contains weights but these are not used by learner %s !", learner$id)
}

