#' @title Check whether the object conatins functional features.
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

hasFunctionalFeatures.data.frame = function(obj) {
  any(vlapply(obj, is.matrix))
}

hasFunctionalFeatures.Task = function(obj) {
  hasFunctionalFeatures.TaskDesc(obj$task.desc)
}

hasFunctionalFeatures.TaskDesc = function(obj) {
  obj$n.feat["functionals"] > 0L
}
