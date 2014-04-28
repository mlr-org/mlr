#' @title Benchmark experiment for multiple learners and tasks.
#'
#' @description
#' Complete benchmark experiment to compare different learning algorithms across one or more tasks
#' w.r.t. a given resampling strategy. Experiments are paired, meaning always the same
#' training / test sets are used for the  different learners.
#'
#' You can also get automatic, internal tuning by using \code{\link{makeTuneWrapper}} with your learner.
#'
#' @param learners [\code{\link{Learner}} | list of them]\cr
#'   Learning algorithms which should be compared.
#' @param tasks [\code{\link{SupervisedTask}} | list of them]\cr
#'   Tasks that learners should be run on.
#' @param resamplings [\code{\link{ResampleDesc}} | \code{\link{ResampleInstance}} | list of them]\cr
#'   Resampling strategies for tasks.
#' @param measures [\code{\link{Measure}} | list of them]\cr
#'   Performance measures.
#' @param same.resampling.instance [logical(1)]\cr
#'   Should the same resampling instance be used for all learners (per task) to reduce variance?
#'   Default is \code{TRUE}.
#' @return [\code{list}].
#' @export
benchmark = function(learners, tasks, resamplings, measures, same.resampling.instance=TRUE) {
  
  # check learners
  if (inherits(learners, "Learner"))
    learners = list(learners)
  checkArg(learners, "list")
  checkListElementClass(learners, "Learner")
  if (!length(learners))
    stop("No learners were passed!")
  learner.ids = extractSubList(learners, "id")
  if (any(duplicated(learner.ids)))
    stop("Learners need unique ids!")
  names(learners) = learner.ids

  # check tasks
  if (inherits(tasks, "SupervisedTask"))
    tasks = list(tasks)
  checkArg(tasks, "list")
  checkListElementClass(tasks, "SupervisedTask")
  if (!length(tasks))
    stop("No tasks were passed!")
  
  task.ids = extractSubList(tasks,"task.desc")["id",]
  if (any(duplicated(task.ids)))
    stop("Tasks need unique ids!")
  names(tasks) = task.ids

  # check resamplings
  if (missing(resamplings))
     resamplings = makeResampleDesc("CV", iters=10L)
  if (inherits(resamplings, "ResampleInstance") || inherits(resamplings, "ResampleDesc"))
    resamplings = replicate(length(tasks), resamplings, simplify=FALSE)
  if (length(resamplings) != length(tasks))
    stop("Number of resampling strategies and number of tasks differ!")
  names(resamplings) = task.ids

  # check measures
  if (missing(measures)) {
    measures = mlr:::default.measures(tasks[[1]])
  } else {
    if (inherits(measures, "Measure"))
     measures = list(measures)
  }

  # check rest
  checkArg(same.resampling.instance, "logical", len=1L, na.ok=FALSE)
  
  measure.ids = extractSubList(measures, "id")
  
  # instantiate resampling
  if (same.resampling.instance) {
    resamplings = Map(function(res, tt) {
      if (is(res, "ResampleInstance"))
        res
      else
        makeResampleInstance(res,task=tt)
    }, resamplings, tasks)
  }

  inds.mat = as.matrix(expand.grid(learner=learner.ids, task=task.ids))
  inds = split(inds.mat, f=seq_row(inds.mat))
  more.args = list(learners=learners, tasks=tasks, resamplings=resamplings, measures=measures)
  results = parallelMap(benchmarkParallel, inds, more.args=more.args)
  res.measures = extractSubList(results, "aggr", simplify=FALSE)
  res.measures = do.call(rbind,res.measures)
  res.df = cbind.data.frame(inds.mat, res.measures)
  
  list(result.df = res.df, results=results)
}

benchmarkParallel = function(index, learners, tasks, resamplings, measures) {
  ind.learner = index[1L][[1]]
  ind.task = index[2L][[1]]
  messagef("Task: %s, Learner: %s", ind.task, ind.learner)
  resample(learners[[ind.learner]], tasks[[ind.task]], resamplings[[ind.task]], measures=measures)
}
