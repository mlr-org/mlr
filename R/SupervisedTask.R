
makeSupervisedTask = function(type, data, target, weights = NULL, blocking = NULL) {
  env = new.env(parent = emptyenv())
  assertDataFrame(data)
  env$data = data
  makeS3Obj(c("SupervisedTask", "Task"),
    env = env,
    weights = weights,
    blocking = blocking,
    task.desc = NA
  )
}

#FIXME: it would probably be better to have: pre-check, fixup, post-check!

checkTaskCreation.SupervisedTask = function(task, target, ...) {
  w = which.first(target %nin% colnames(task$env$data))
  if (length(w) > 0L)
    stopf("Column names of data doesn't contain target var: %s", target[w])
  for (tt in target) {
    if (any(is.na(task$env$data[[tt]])))
      stopf("Target column '%s' contains missing values!", tt)
  }
  NextMethod("checkTaskCreation")
}

fixupData.SupervisedTask = function(task, target, choice) {
  NextMethod("fixupData")
}

#' @export
print.SupervisedTask = function(x, print.target = TRUE, print.weights = TRUE, ...) {
  td = x$task.desc
  catf("Supervised task: %s", td$id)
  catf("Type: %s", td$type)
  if (print.target)
    catf("Target: %s", collapse(td$target))
  catf("Observations: %i", td$size)
  catf("Features:")
  catf(printToChar(td$n.feat, collapse = "\n"))
  catf("Missings: %s", td$has.missings)
  if (print.weights)
    catf("Has weights: %s", td$has.weights)
  catf("Has blocking: %s", td$has.blocking)
}
