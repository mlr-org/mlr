makeSupervisedTask = function(type, data, target, weights, blocking, fixup.data, check.data, coordinates) {
  task = makeTask(type = type, data = data, weights = weights, blocking = blocking, fixup.data = fixup.data, check.data = check.data, coordinates = coordinates)

  if (check.data) {
    # costsens does not have a target col...
    # assertCharacter(target, any.missing = FALSE, min.len = 1L)
    w = which.first(target %nin% colnames(data))
    if (length(w) > 0L) {
      stopf("Column names of data doesn't contain target var: %s", target[w])
    }
    checkTaskData(task$env$data, cols = setdiff(colnames(data), target))
  }

  addClasses(task, "SupervisedTask")
}

#' @export
print.SupervisedTask = function(x, print.target = TRUE, print.weights = TRUE, ...) {

  td = x$task.desc
  catf("Supervised task: %s", td$id)
  catf("Type: %s", td$type)
  if (print.target) {
    catf("Target: %s", collapse(td$target))
  }
  if (inherits(x, "SurvTask")) {
    catf("Events: %i", sum(getTaskTargets(x)[, 2L]))
  }
  catf("Observations: %i", td$size)
  catf("Features:")
  catf(printToChar(td$n.feat, collapse = "\n"))
  catf("Missings: %s", td$has.missings)
  if (print.weights) {
    catf("Has weights: %s", td$has.weights)
  }
  catf("Has blocking: %s", td$has.blocking)
  catf("Has coordinates: %s", td$has.coordinates)
}
