#' @title Create a survival task.
#' @inheritParams Task
#' @seealso [Task] [ClassifTask] [ClusterTask] [CostSensTask] [MultilabelTask] [RegrTask]
#' @rdname SurvTask
#' @aliases SurvTask
#' @export
makeSurvTask = function(id = deparse(substitute(data)), data, target, weights = NULL, blocking = NULL, coordinates = NULL, fixup.data = "warn", check.data = TRUE) {

  assertString(id)
  assertDataFrame(data)
  assertCharacter(target, any.missing = FALSE, len = 2L)
  assertChoice(fixup.data, choices = c("no", "quiet", "warn"))
  assertFlag(check.data)


  if (fixup.data != "no") {
    time = data[[target[1L]]]
    event = data[[target[2L]]]

    if (is.integer(time)) {
      data[[target[1L]]] = as.double(time)
    }

    if (is.numeric(event)) {
      if (testIntegerish(event) && all(as.integer(event) %in% c(0L, 1L))) {
        data[[target[2L]]] = (as.integer(event) == 1L)
      }
    } else if (is.factor(event)) {
      lvls = levels(event)
      if (length(lvls) == 2L) {
        if (all(lvls %in% c("TRUE", "FALSE"))) {
          data[[target[2L]]] = (event == "TRUE")
        } else if (all(lvls %in% c("0", "1"))) {
          data[[target[2L]]] = (as.character(event) == "1")
        }
      }
    }
  }

  task = makeSupervisedTask("surv", data, target, weights, blocking, coordinates, fixup.data = fixup.data, check.data = check.data)

  if (check.data) {
    time = data[[target[1L]]]
    event = data[[target[2L]]]
    assertNumeric(time, lower = 0, finite = TRUE, any.missing = FALSE, .var.name = "target column time")
    assertLogical(event, any.missing = FALSE, .var.name = "target column event")
  }
  task$task.desc = makeSurvTaskDesc(id, data, target, weights, blocking, coordinates)
  addClasses(task, "SurvTask")
}

#' @export
#' @rdname makeTaskDesc
makeSurvTaskDesc = function(id, data, target, weights, blocking, coordinates) {
  td = makeTaskDescInternal("surv", id, data, target, weights, blocking, coordinates)
  addClasses(td, c("SurvTaskDesc", "SupervisedTaskDesc"))
}
