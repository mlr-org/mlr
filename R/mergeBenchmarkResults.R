#' @title Merge different BenchmarkResult objects.
#' @description Combines the \code{\link{BenchmarkResult}} objects that were performed 
#'   with different learners and tasks. 
#'   This can be helpful if you, e.g. forgot to run one learner on the set of tasks you used.
#' @param ... [\code{\link{BenchmarkResult}}]\cr 
#'   \code{BenchmarkResult} objects that should be merged.
#' @return \code{\link{BenchmarkResult}}
#' @details Note that if you want to merge several \code{\link{BenchmarkResult}}
#'   objects you must ensure that all possible learner and task combinations will be
#'   contained in the return object.\cr
#'   Furthermore all given objects must have been calculated on the same
#'   set of measures.
#' @export
mergeBenchmarkResults = function(...) {
  set = list(...)
  for (i in 1:length(set)) {
    assertClass(set[[i]], "BenchmarkResult")
  }
  # Check if bmrs optimized on same measure
  measures = lapply(set, function(bmr) {
    getBMRMeasureIds(bmr)
  })
  if (length(unique(measures)) > 1L)
    stop("Benchmark results must all be calculated for the same measures.")

  task.names = lapply(set, getBMRTaskIds)
  task.names = unique(unlist(task.names))
  learner.names = lapply(set, getBMRLearnerIds)
  learner.names = unique(unlist(learner.names))

  all.combos = expand.grid(task = task.names, learner = learner.names)
  all.combos = apply(all.combos, 1L, collapse, "-")
  
  # see what combinations of tasks and learners are existing
  existing.combos = lapply(set, function(bmr) {
    getBMRAggrPerformances(bmr, as.df = TRUE)[, c("task.id", "learner.id")]
  })
  existing.combos = do.call("rbind", existing.combos)
  existing.combos = apply(existing.combos, 1L, collapse, "-")
  
  # check for duplicated and missing task - learner combinations
  if (any(duplicated(existing.combos))) {
    dupls = existing.combos[which(duplicated(existing.combos))]
    stopf("The following task - learner combination(s) occur in multiple
      benchmark experiments: \n-%s\n",
      collapse(dupls, "\n-"))
  }
  if (length(existing.combos) < length(all.combos)) {
    diff = setdiff(all.combos, existing.combos)
    stopf("The following task - learner combination(s) are missing: \n-%s\n",
      collapse(diff, "\n-"))
  }

  # get BMR results, merge and set correct structure
  res.merged = lapply(set, function(bmr) bmr$results)
  res.merged = peelList(res.merged)
  res.merged = lapply(task.names, function(x) {
    ret = res.merged[names(res.merged) == x]
    names(ret) = NULL
    peelList(ret)
  })
  names(res.merged) = task.names
  res.merged = res.merged[order(names(res.merged))]
  
  # get all actual learners and merge
  lrns.merged = peelList(lapply(set, getBMRLearners))
  lrns.merged = lrns.merged[!duplicated(lrns.merged)]

  makeS3Obj("BenchmarkResult",
    results = res.merged,
    measures = getBMRMeasures(set[[1L]]),
    learners = lrns.merged)
}

# simple wrapper for unlist() with recursive set to FALSE
peelList = function(x) {
  unlist(x, recursive = FALSE)
}
