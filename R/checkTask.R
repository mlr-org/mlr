checkTask = function(task, cl = "SupervisedTask", task.type = NULL, binary = FALSE) {
  assertClass(task, classes = cl)
  if (!is.null(task.type) && task$task.desc$type %nin% task.type)
    stopf("Task must be one of '%s', but is: '%s'", collapse(task.type), task$task.desc$type)
  if (binary && length(task$task.desc$class.levels) != 2L)
    stopf("Task '%s' must be binary classification!", task$task.desc$id)
}
