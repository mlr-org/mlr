#' Find matching measures.
#'
#' Returns the matching measures which have specific characteristics, e.g.
#' whether they supports classification or regression.
#'
#' @param properties [\code{character)}]\cr
#'   Set of required properties to filter for. Default is \code{character(0)}.
#'   See \link{Measure} for some standardized properties.
#' @return [\code{character}]. Class names of matching learners.
##' @export
listMeasures = function(properties = character(0L)) {
  checkArg(properties, "character", na.ok = FALSE)
  ee = as.environment("package:mlr")
  res = vlapply(ee, function(x) inherits(x, "Measure") && all(properties %in% x$properties))
  names(res)[res]
}

#' @param task [\code{\link{SupervisedTask}}]\cr
#'   The task. Learners are returned that are applicable.
#' @rdname listLearners
##' @export
listMeasuresForTask = function(task, properties = character(0L)) {
  checkArg(task, "SupervisedTask")
  checkArg(properties, "character", na.ok = FALSE)
  td = task$task.desc

  props = td$type
  if (td$type == "classif" && length(td$class.levels) > 2L)
    props = c(props, "classif.multi")
  listMeasures(properties = props)
}
