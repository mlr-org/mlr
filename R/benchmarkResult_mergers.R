#' @title Merge different learners of BenchmarkResult objects.
#' @description Combines the \code{\link{BenchmarkResult}} objects that were performed 
#'   with different learners on the same set of Task(s). 
#'   This can be helpful if you, e.g. forgot to run one learner on the set of tasks you used.
#' @param ... [\code{\link{BenchmarkResult}}]\cr 
#'   \code{BenchmarkResult} objects that should be merged.
#' @export
mergeBenchmarkResultLearner = function(...) {
  mergeLearner = function(x, y) {
    if (!sameMeasures(x, y)) 
      stop("merging 'BenchmarkResult's must be based on the same set of measures")
    if (any(names(x$learners)%in%names(y$learners))) 
      stop("duplicated learner on same task found")
    if (identical(sort(names(x$results)), sort(names(y$results)))) {
      results = Map(append, x$results, y$results[names(x$results)])
    } else stop("merging different learners of 'BenchmarkResult's must be based on the same set of tasks")
    learners = Map(append, x, y)$learners
    
    makeS3Obj("BenchmarkResult",
              results = results,
              measures = x$measures,
              learners = learners
    )
  }
  Reduce(function(...) mergeLearner(...), list(...))
}

#' @title Merge different tasks of BenchmarkResult objects.
#' @description Combines the \code{\link{BenchmarkResult}} objects that were performed
#'   on different tasks with the same set of learner(s). 
#'   This can be helpful if you, e.g. forgot to run the set of learners on a new task
#' @param ... [\code{\link{BenchmarkResult}}]\cr 
#'   \code{BenchmarkResult} objects that should be merged.
#' @export
mergeBenchmarkResultTask = function(...) {
  mergeTask = function(x, y) {
    if (!sameMeasures(x, y)) 
      stop("merging 'BenchmarkResult's must be based on the same set of measures")
    if (any(names(x$results)%in%names(y$results))) 
      stop("duplicated task found")
    if (identical(sort(names(x$learners)), sort(names(y$learners)))) {
      results = Map(append, x, y)$results
    } else stop("merging different tasks of 'BenchmarkResult's must be based on the same set of learners")
    
    makeS3Obj("BenchmarkResult",
              results = results,
              measures = x$measures,
              learners = x$learners
    )
  }
  Reduce(function(...) mergeTask(...), list(...))
}

# Checks if same measures are used in two different BenchmarkResult objects
sameMeasures = function(x, y) {
  meas1 = vcapply(x$measures, function(X) X$id)
  meas2 = vcapply(y$measures, function(X) X$id)
  return (identical(sort(meas1), sort(meas2)))
}

