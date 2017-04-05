ensureBenchmarkLearners = function(learners) {
  learners = ensureVector(learners, 1L, "Learner")
  learners = lapply(learners, checkLearner)
  learner.ids = vcapply(learners, getLearnerId)
  if (anyDuplicated(learner.ids))
    stop("Learners need unique ids!")
  learners
}

ensureBenchmarkTasks = function(tasks) {
  tasks = ensureVector(tasks, 1L, "Task")
  assertList(tasks, min.len = 1L)
  checkListElementClass(tasks, "Task")
  task.ids = vcapply(tasks, getTaskId)
  if (anyDuplicated(task.ids))
    stop("Tasks need unique ids!")
  tasks
}

ensureBenchmarkResamplings = function(resamplings, tasks) {
  if (missing(resamplings)) {
    resamplings = replicate(length(tasks), makeResampleDesc("CV", iters = 10L), simplify = FALSE)
  } else if (inherits(resamplings, "ResampleInstance") || inherits(resamplings, "ResampleDesc")) {
    resamplings = replicate(length(tasks), resamplings, simplify = FALSE)
  } else {
    assertList(resamplings)
    if (length(resamplings) != length(tasks))
      stop("Number of resampling strategies and number of tasks differ!")
  }
  resamplings
}

ensureBenchmarkMeasures = function(measures, tasks) {
  if (missing(measures)) {
    measures = list(getDefaultMeasure(tasks[[1L]]))
  } else {
    measures = ensureVector(measures, 1L, "Measure")
    assertList(measures)
    checkListElementClass(measures, "Measure")
  }
  measures
}

# get extractor function for different wrapped models
getExtractor = function(lrn) {
  cl = class(lrn)
  if ("FeatSelWrapper" %in% cl) {
    extract.this = getFeatSelResult
  } else if ("TuneWrapper" %in% cl) {
    extract.this = getTuneResult
  } else if ("FilterWrapper" %in% cl) {
    extract.this = getFilteredFeatures
  } else {
    extract.this = function(model) { NULL }
  }
  extract.this
}
