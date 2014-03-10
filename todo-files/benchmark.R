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
#' @param models [logical] \cr
#'   Should all fitted models be stored?
#'   Default is FALSE.
#' @param same.resampling.instance [logical(1)]\cr
#'   Should the same resampling instance be used for all learners (per task) to reduce variance?
#'   Default is \code{TRUE}.
#' @return \code{\linkS4class{bench.result}}.
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

  # check tasks
  if (inherits(tasks, "SupervisedTask"))
    tasks = list(tasks)
  checkArg(tasks, "list")
  checkListElementClass(tasks, "SupervisedTask")
  if (!length(tasks))
    stop("No tasks were passed!")
  task.ids = extractSubList(tasks, "id")
  if (any(duplicated(task.ids)))
    stop("Tasks need unique ids!")

  # check resamplings
  if (missing(resamplings))
     resamplings = makeResampleDesc("CV", iters=10L)
  if (inherits(resamplings, "ResampleInstance") || inherits(resamplings, "ResampleDesc"))
    resamplings = replicate(length(tasks), resamplings, simplify=FALSE)
  if (length(resamplings) != length(tasks))
    stop("Number of resampling strategies and number of tasks differ!")

  # check measures
  if (missing(measures)) {
    measures = default.measures(tasks[[1]])
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

  tds = ins = rrs = ors = list()

  inds = as.matrix(expand.grid(1:length(learners), 1:length(tasks)))
  inds = lapply(1:nrow(inds), function(i) inds[i, ])
  more.args = list(learners=learners, tasks=tasks, resamplings=resamplings, measures=measures)
  parallelMap(benchmarkParallel, inds, more.args=more.args)
  # results = parallelMap(inds, benchmark_par, from="bench", learners=learners, tasks=tasks, resamplings=resamplings,
    # measures=measures, models=models)

  counter = 1
  for (j in 1:length(tasks)) {
    task = tasks[[j]]
    rrs[[j]] = list()
    ors[[j]] = list()
    tds[[j]] = task@desc
    for (i in 1:length(learners)) {
      wl = learners[[i]]
      learner.names[i] = wl@desc@id
      bm = results[[counter]]
      counter = counter+1

      rrs[[j]][[i]] = bm$res.result
      if(is(wl, "OptWrapper")) ors[[j]][[i]] = bm$ors else ors[[j]][i] = list(NULL)
    }
    names(rrs[[j]]) = learner.names
    names(ors[[j]]) = learner.names
  }
  names(tds) = task.names
  names(learners) = learner.names
  names(resamplings) = task.names
  names(rrs) = task.names
  names(ins) = task.names
  return(new("bench.result", task.descs=tds, learners=learners, resamplings=resamplings,
      measures=measures, res.results = rrs, opt.results = ors, input.names=ins
    ))
}

benchmarkParallel = function(index, learners, tasks, resamplings, measures) {
  ind.learner = index[1L]
  ind.task = index[2L]
  resample(learners[[ind.learner]], tasks[[ind.task]], resamplings[[ind.task]], measures=measures)
}
