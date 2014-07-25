makeUnsupervisedTask = function(type, data, weights = NULL, blocking = NULL) {
  env = new.env(parent = emptyenv())
  assertDataFrame(data)
  env$data = data
  makeS3Obj(c("UnsupervisedTask", "Task"),
    env = env,
    weights = weights,
    blocking = blocking,
    task.desc = NA
  )
}

checkTaskCreation.UnsupervisedTask = function(task, ...) {
  NextMethod("checkTaskCreation")
}

fixupData.UnsupervisedTask = function(task, target, choice) {
  NextMethod("fixupData")
}

#' @export
print.UnsupervisedTask = function(x, print.weights = TRUE, ...) {
  td = x$task.desc
  catf("Unsupervised task: %s", td$id)
  catf("Type: %s", td$type)
  catf("Observations: %i", td$size)
  catf("Features:")
  catf(printToChar(td$n.feat, collapse = "\n"))
  catf("Missings: %s", td$has.missings)
  if (print.weights)
    catf("Has weights: %s", td$has.weights)
  catf("Has blocking: %s", td$has.blocking)
}
