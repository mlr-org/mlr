#' @title Find matching measures.
#'
#' @description
#' Returns the matching measures which have specific characteristics, e.g.
#' whether they supports classification or regression.
#'
#' @template arg_task_or_type
#' @param properties ([character])\cr
#'   Set of required properties to filter for.
#'   See [Measure] for some standardized properties.
#'   Default is `character(0)`.
#' @param create (`logical(1)`)\cr
#'   Instantiate objects (or return strings)?
#'   Default is `FALSE`.
#' @return ([character` | `list` of [Measure]). Class names of matching
#'   measures or instantiated objects.
#' @export
listMeasures = function(obj, properties = character(0L), create = FALSE) {
  if (!missing(obj)) {
    assert(checkCharacter(obj), checkClass(obj, "Task"))
  }
  assertSubset(properties, listMeasureProperties())
  assertFlag(create)
  UseMethod("listMeasures")
}

#' @rdname listMeasures
#' @export
listMeasures.default = function(obj, properties = character(0L), create = FALSE) {
  listMeasures2(properties, create)
}

#' @rdname listMeasures
#' @export
listMeasures.character = function(obj, properties = character(0L), create = FALSE) {
  assertChoice(obj, choices = c("classif", "multilabel", "regr", "surv", "costsens", "cluster", NA_character_))
  if (is.na(obj)) {
    obj = character(0L)
  }
  listMeasures2(union(obj, properties), create)
}

#' @rdname listMeasures
#' @export
listMeasures.Task = function(obj, properties = character(0L), create = FALSE) {
  td = obj$task.desc
  if (td$type == "classif" && length(td$class.levels) > 2L) {
    properties = union(properties, "classif.multi")
  }
  listMeasures.character(td$type, properties = properties, create)
}

listMeasures2 = function(properties = character(0L), create = FALSE) {
  ee = as.environment("package:mlr")
  res = Filter(function(x) inherits(x, "Measure") && all(properties %in% getMeasureProperties(x)), as.list(ee))
  if (create) {
    res
  } else {
    names(res)
  }
}
