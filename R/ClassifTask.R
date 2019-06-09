#' @title Create a classification task.
#' @inheritParams Task
#' @seealso [Task] [CostSensTask] [ClusterTask] [MultilabelTask] [RegrTask] [SurvTask]
#' @rdname ClassifTask
#' @aliases ClassifTask
#' @export
makeClassifTask = function(id = deparse(substitute(data)), data, target, weights = NULL, blocking = NULL, coordinates = NULL, positive = NA_character_, fixup.data = "warn", check.data = TRUE) {

  assertString(id)
  assertDataFrame(data)
  assertString(target)
  # some code on cran passed stuff like positive=1, we can live with the convert here
  if (isScalarNumeric(positive)) {
    positive = as.character(positive)
  }

  assertString(positive, na.ok = TRUE)
  assertChoice(fixup.data, choices = c("no", "quiet", "warn"))
  assertFlag(check.data)

  if (fixup.data != "no") {
    x = data[[target]]
    if (is.character(x) || is.logical(x) || is.integer(x)) {
      data[[target]] = as.factor(x)
    } else if (is.factor(x) && fixup.data == "warn" && hasEmptyLevels(x)) {
      warningf("Target column '%s' contains empty factor levels", target)
      data[[target]] = droplevels(x)
    }
  }
  task = makeSupervisedTask("classif", data, target, weights, blocking, coordinates, fixup.data = fixup.data, check.data = check.data)

  if (check.data) {
    assertFactor(data[[target]], any.missing = FALSE, empty.levels.ok = FALSE, .var.name = target)
  }

  task$task.desc = makeClassifTaskDesc(id, data, target, weights, blocking, positive, coordinates)
  addClasses(task, "ClassifTask")
}

#' @export
#' @rdname makeTaskDesc
makeClassifTaskDesc = function(id, data, target, weights, blocking, positive, coordinates) {

  levs = levels(data[[target]])
  m = length(levs)
  if (is.na(positive)) {
    if (m <= 2L) {
      positive = levs[1L]
    }
  } else {
    if (m > 2L) {
      stop("Cannot set a positive class for a multiclass problem!")
    }
    assertChoice(positive, choices = levs)
  }
  td = makeTaskDescInternal("classif", id, data, target, weights, blocking, coordinates)
  td$class.levels = levs
  td$positive = positive
  td$negative = NA_character_
  td$class.distribution = table(data[target])
  if (length(td$class.levels) == 1L) {
    td$negative = stri_paste("not_", positive)
  } else if (length(td$class.levels) == 2L) {
    td$negative = setdiff(td$class.levels, positive)
  }
  return(addClasses(td, c("ClassifTaskDesc", "SupervisedTaskDesc")))
}

#' @export
print.ClassifTask = function(x, ...) {
  di = printToChar(x$task.desc$class.distribution)
  m = length(x$task.desc$class.levels)
  print.SupervisedTask(x)
  catf("Classes: %i", m, newline = FALSE)
  # remove 1st newline
  cat(di)
  catf("\nPositive class: %s", x$task.desc$positive)
}
