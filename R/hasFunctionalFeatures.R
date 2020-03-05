#' @title Check whether the object contains functional features.
#'
#' @description
#' See title.
#'
#' @param obj (`Task` | `TaskDesc` | `data.frame`)\cr
#'   Object to check.
#' @return (`logical(1)`)
#' @export
hasFunctionalFeatures = function(obj) {
  UseMethod("hasFunctionalFeatures")
}

#' @export
hasFunctionalFeatures.data.frame = function(obj) {
  any(vlapply(obj, is.matrix))
}

#' @export
hasFunctionalFeatures.Task = function(obj) {
  obj$task.desc$n.feat["functionals"] > 0L
}
