#' @export
#' @rdname SupervisedTask
makeClassifTask = function(id, data, target, blocking, positive, check.data=TRUE) {
  task = makeSupervisedTask("classif", id, data, target, blocking, positive, check.data)
  return(addClasses(task, "ClassifTask"))
}

#' @S3method print ClassifTask
print.ClassifTask = function(x, ...) {
  # remove 1st newline
  di = printToChar(table(getTaskTargets(x)), collapse=NULL)[-1L]
  m = length(x$task.desc$class.levels)
  print.SupervisedTask(x)
  catf("Classes: %i", m)
  catf(collapse(di, "\n"))
  catf("Positive class: %s", x$task.desc$positive)
}
