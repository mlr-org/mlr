#' @title Find matching measures.
#'
#' @description
#' Returns the matching measures which have specific characteristics, e.g.
#' whether they supports classification or regression.
#'
#' \code{listMeasuresForTask} returns all learners that are in principle applicable
#' for a given task.
#'
#' @template arg_task_or_type
#' @param properties [\code{character)}]\cr
#'   Set of required properties to filter for.
#'   See \code{\link{Measure}} for some standardized properties.
#'   Default is \code{character(0)}.
#' @param create [\code{logical(1)}]\cr
#'   Instantiate objects (or return strings)?
#'   Default is \code{FALSE}.
#' @return [\code{character} | \code of \code{\link{Measure}}]. Class names of matching
#'   measures or instantiated objects.
#' @export
listMeasures = function(obj, properties = character(0L), create = FALSE) {
  if (!missing(obj))
    checkArg(obj, c("character", "SupervisedTask"))
  checkArg(properties, "character", na.ok = FALSE)
  checkArg(create, "logical", len = 1L, na.ok = FALSE)
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
  checkArg(obj, choices = c("classif", "regr", "surv", "costsens", NA_character_))
  if (is.na(obj))
    obj = character(0L)
  listMeasures2(union(obj, properties), create)
}

#' @rdname listMeasures
#' @export
listMeasures.SupervisedTask = function(obj, properties = character(0L), create = FALSE) {
  td = obj$task.desc
  if (td$type == "classif" && length(td$class.levels) > 2L)
    properties = union(properties, "classif.multi")
  listMeasures.character(td$type, properties = properties, create)
}

listMeasures2 = function(properties = character(0L), create = FALSE) {
  ee = as.environment("package:mlr")
  res = Filter(function(x) inherits(x, "Measure") && all(properties %in% x$properties), as.list(ee))
  if (create)
    res
  else
    names(res)
}
