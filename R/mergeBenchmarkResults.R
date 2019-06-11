#' @title Merge different BenchmarkResult objects.
#' @description The function automatically combines a list of [BenchmarkResult]
#'   objects into a single [BenchmarkResult] object as long as the full
#'   crossproduct of all task-learner combinations are available.
#' @param bmrs [list of [BenchmarkResult])\cr
#'   `BenchmarkResult` objects that should be merged.
#' @return [BenchmarkResult]
#' @details Note that if you want to merge several [BenchmarkResult]
#'   objects, you must ensure that all possible learner and task combinations will be
#'   contained in the returned object. Otherwise, the user will be notified which
#'   task-learner combinations are missing or duplicated.
#'   When merging [BenchmarkResult] objects with different measures,
#'   all missing measures will automatically be recomputed.
#' @noMd
#' @export
mergeBenchmarkResults = function(bmrs) {

  # check all objects have the class BenchmarkResult
  assertList(bmrs, types = "BenchmarkResult")

  # check if all task types are equal
  unique.tt = unique(unlist(lapply(bmrs, function(x) getBMRObjects(x, fun = getTaskType))))
  if (length(unique.tt) != 1) {
    stopf("Different task types found: %s", collapse(unique.tt))
  }

  # check if resample descriptions are equal for each task
  task.rin = peelList(lapply(bmrs, function(bmr) getBMRObjects(bmr, fun = function(x) getRRPredictions(x)$instance$desc)))
  task.rin = groupNamedListByNames(task.rin)
  unique.rin = vlapply(task.rin, function(x) length(unique(x)) == 1)
  if (any(!unique.rin)) {
    stopf("Different resample description found for tasks: %s", collapse(names(unique.rin)[!unique.rin]))
  }

  # get unique learner ids and task ids
  learner.ids = unique(unlist(lapply(bmrs, getBMRLearnerIds)))
  task.ids = unique(unlist(lapply(bmrs, getBMRTaskIds)))

  # check for duplicated or missing task-learner combinations
  all.combos = expand.grid(task.id = task.ids, learner.id = learner.ids)
  all.combos = stri_paste(all.combos$task.id, all.combos$learner.id, sep = " - ")
  existing.combos = rbindlist(lapply(bmrs, function(bmr) {
    getBMRAggrPerformances(bmr, as.df = TRUE)[, c("task.id", "learner.id")]
  }), use.names = TRUE)
  existing.combos = stri_paste(existing.combos$task.id, existing.combos$learner.id, sep = " - ")
  if (!identical(sort(existing.combos), sort(all.combos))) {
    dupls = existing.combos[duplicated(existing.combos)]
    diff = setdiff(all.combos, existing.combos)
    msg = collapse(unique(c(dupls, diff)), "\n* ")
    # FIXME: instead of exiting with error, we might allow this and add NA to the missing task-learner combos, check plot funs if we allow this.
    stopf("The following task - learner combination(s) occur either multiple times or are missing: \n* %s\n", msg)
  }

  # get all learners from bmrs and merge
  lrns.merged = peelList(lapply(bmrs, getBMRLearners))
  lrns.merged = unique(lrns.merged) # lrns.merged[!duplicated(lrns.merged)]

  # get ResampleResults from bmrs and merge them by setting the correct structure
  res.merged = peelList(extractSubList(bmrs, "results", simplify = FALSE))
  res.merged = groupNamedListByNames(res.merged)

  # get all unique measures used in the bmr objects and recompute missing measures in RR
  measures.merged = peelList(lapply(bmrs, getBMRMeasures))
  measures.merged = unique(measures.merged) # measures.merged[!duplicated(measures.merged)]
  for (i in seq_along(res.merged)) {
    for (j in seq_along(res.merged[[i]])) {
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

groupNamedListByNames = function(xs, name = sort(unique(names(xs)))) {
  assertList(xs, names = "named")
  assertCharacter(name)

  res = lapply(name, function(x) {
    ret = xs[names(xs) == x]
    names(ret) = NULL
    peelList(ret)
  })
  names(res) = name
  res[order(names(res))]
}
