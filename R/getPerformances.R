#' Extract performance measures of bechmark result.
#'
#' @template arg_bmr
#' @return [\code{data.frame}].
#' @export
#' @family benchmark
getPerformances = function(object) {
  UseMethod("getPerformances")
}

#' @export
getPerformances.BenchmarkResult = function(object) {
  extractMeasures = function(learner.name, task.name) {
      x = dropNamed(object[[task.name]][[learner.name]]$measures.test, "iter")
      setNames(x, paste0(learner.name, ".", names(x)))
  }
  setNames(lapply(names(object), function(task.name) {
    cbind(
      object[[task.name]][[1L]]$measures.test[, "iter", drop = FALSE],
      do.call(cbind, lapply(names(object[[task.name]]), extractMeasures, task.name = task.name))
    )
  }), names(object))
}

