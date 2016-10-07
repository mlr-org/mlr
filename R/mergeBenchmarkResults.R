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
  # check all objects have the class BenchmarkResult
  set = list(...)
  assertList(set, types = "BenchmarkResult")

  # check for duplicated and missing task-learner combinations
  learner.names = unique(unlist(lapply(set, getBMRLearnerIds)))
  task.names = unique(unlist(lapply(set, getBMRTaskIds)))
  all.combos = expand.grid(task = task.names, learner = learner.names)
  all.combos = apply(all.combos, 1L, collapse, " - ")
  existing.combos = rbindlist(lapply(set, function(bmr) {
    getBMRAggrPerformances(bmr, as.df = TRUE)[, c("task.id", "learner.id")]
  }))
  existing.combos = apply(existing.combos, 1L, collapse, " - ")
  if (!identical(sort(existing.combos), sort(all.combos))) {
    dupls = existing.combos[duplicated(existing.combos)]
    diff = setdiff(all.combos, existing.combos)
    msg = collapse(unique(c(dupls, diff)), "\n* ")
    # FIXME: instead of exiting with error, we might allow this and add NA to the missing task-learner combos, check plot funs if we allow this.
    stopf("The following task - learner combination(s) occur either multiple times or are missing: \n* %s\n", msg)
  }

  # check if all task.desc are equal for each task and stop if there are tasks with more than 1 unique td
  td = peelList(lapply(set, getBMRTaskDescriptions))
  inequal.td = vlapply(td, function(x) length(unique(x)) > 1)
  if (any(inequal.td))
    stopf("Task descriptions not equal for tasks: %s", collapse(names(inequal.td)[!inequal.td]))

  # check if BMR use same measures
  # measures = lapply(set, function(x) unique(getBMRMeasureIds(x)))
  # intersect.measures = Reduce(intersect, measures)
  # all.measures = unique(peelList(lapply(set, getBMRMeasures)))
  # names(all.measures) = vcapply(all.measures, function(x) x$id)
  # measures.merged = unname(all.measures[intersect.measures])
  measures.merged = peelList(lapply(set, getBMRMeasures))
  measures.merged = measures.merged[!duplicated(measures.merged)]

  # get all actual learners and merge
  lrns.merged = peelList(lapply(set, getBMRLearners))
  lrns.merged = lrns.merged[!duplicated(lrns.merged)]

  # get BMR results, merge and set correct structure
  res.merged = peelList(extractSubList(set, "results", simplify = FALSE))
  res.merged = lapply(task.names, function(x) {
    ret = res.merged[names(res.merged) == x]
    names(ret) = NULL
    peelList(ret)
  })
  names(res.merged) = task.names
  res.merged = res.merged[order(names(res.merged))]
  # recompute missing measures
  for(i in 1:length(res.merged)) {
    for(j in 1:length(res.merged[[i]])) {
      res.merged[[i]][[j]] = addRRMeasure(res.merged[[i]][[j]], measures.merged)
    }
  }

  makeS3Obj("BenchmarkResult",
    results = res.merged,
    measures = measures.merged,
    learners = lrns.merged)
}

# simple wrapper for unlist() with recursive set to FALSE
peelList = function(x) {
  unlist(x, recursive = FALSE)
}
