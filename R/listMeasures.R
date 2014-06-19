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
#' @return [\code{character}]. Class names of matching learners.
#' @export
listMeasures = function(obj, properties = character(0L)) {
  if (!missing(obj))
    checkArg(obj, c("character", "SupervisedTask"))
  checkArg(properties, "character", na.ok = FALSE)
  UseMethod("listMeasures")
}

#' @rdname listMeasures
#' @export
listMeasures.default = function(obj, properties = character(0L)) {
  listMeasures2(properties)
}

#' @rdname listMeasures
#' @export
listMeasures.character = function(obj, properties = character(0L)) {
  checkArg(obj, choices = c("classif", "regr", "surv", "costsens", NA_character_))
  if (is.na(obj))
    obj = character(0L)
  listMeasures2(union(obj, properties))
}

#' @rdname listMeasures
#' @export
listMeasures.SupervisedTask = function(obj, properties = character(0L)) {
  td = obj$task.desc
  if (td$type == "classif" && length(td$class.levels) > 2L)
    properties = union(properties, "classif.multi")
  listMeasures.character(td$type, properties = properties)
}

listMeasures2 = function(properties = character(0L)) {
  ee = as.environment("package:mlr")
  res = vlapply(ee, function(x) inherits(x, "Measure") && all(properties %in% x$properties))
  names(res)[res]
}
