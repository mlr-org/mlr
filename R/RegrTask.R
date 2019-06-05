#' @title Create a regression task.
#' @inheritParams Task
#' @seealso [Task] [ClassifTask] [CostSensTask] [ClusterTask] [MultilabelTask] [SurvTask]
#' @rdname RegrTask
#' @aliases RegrTask
#' @export
makeRegrTask = function(id = deparse(substitute(data)), data, target, weights = NULL, blocking = NULL, coordinates = NULL, fixup.data = "warn", check.data = TRUE) {

  assertString(id)
  assertDataFrame(data)
  assertString(target)
  assertChoice(fixup.data, choices = c("no", "quiet", "warn"))
  assertFlag(check.data)

  if (fixup.data != "no") {
    if (is.integer(data[[target]])) {
      data[[target]] = as.double(data[[target]])
    }
  }

  task = makeSupervisedTask("regr", data, target, weights, blocking, coordinates, fixup.data = fixup.data, check.data = check.data)

  if (check.data) {
    assertNumeric(data[[target]], any.missing = FALSE, finite = TRUE, .var.name = target)
  }

  task$task.desc = makeRegrTaskDesc(id, data, target, weights, blocking, coordinates)
  addClasses(task, "RegrTask")
}

#' @export
#' @rdname makeTaskDesc
makeRegrTaskDesc = function(id, data, target, weights, blocking, coordinates) {
  addClasses(makeTaskDescInternal("regr", id, data, target, weights, blocking, coordinates), c("RegrTaskDesc", "SupervisedTaskDesc"))
}
