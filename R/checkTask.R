# performs arg checks of a task (or maybe also allow an taskdesc)
# you can check that the task is from a list of certain types
checkTask = function(x, cl = "Task", allow.desc = FALSE, task.type = NULL, binary = FALSE, .var.name = "task") {
  if (allow.desc) {
    assert(.var.name = .var.name,
      checkClass(x, classes = cl),
      checkClass(x, "TaskDesc")
    )
  } else {
    assertClass(x, classes = cl, .var.name = .var.name)
  }
  td = getTaskDesc(x)

  if (!is.null(task.type) && td$type %nin% task.type) {
    stopf("Task must be one of '%s', but is: '%s'", collapse(task.type), td$type)
  }
  if (binary && length(td$class.levels) != 2L) {
    stopf("Task '%s' must be binary classification!", td$id)
  }
}
