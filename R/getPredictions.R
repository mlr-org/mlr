#' Extract the predictions from a benchmark result.
#'
#' @template arg_bmr
#' @return [\code{data.frame}].
#' @export
#' @family benchmark
getPredictions = function(object) {
  UseMethod("getPredictions")
}

#' @export
getPredictions.BenchmarkResult = function(object) {
  extractResponse = function(learner.name, task.name) {
    setNames(data.frame(object[[task.name]][[learner.name]]$pred)[, "response", drop = FALSE],
      paste0("response.", learner.name))
  }
  setNames(lapply(names(object), function(task.name) {
    cbind(
      as.data.frame(object[[task.name]][[1L]]$pred)[, c("id", "truth", "iter", "set"), drop = FALSE],
      do.call(cbind, lapply(names(object[[task.name]]), extractResponse, task.name = task.name))
    )
  }), names(object))
}


