#init for dev
library(devtools)
library(checkmate)
load_all()
source("tests/testthat/helper_objects.R")
lrns = list("classif.rpart", "classif.knn", "classif.svm")
task = iris.task
measures = list(mmce, acc)
resampling = NULL
percs = seq(0.1, 1, by = 0.1)
stratify = FALSE

#'
#' @param resampling [\code{\link{ResampleDesc}} or \code{\link{ResampleInstance}}]\cr
#'   Resampling strategy.
#'   If a description is passed, it is instantiated automatically.
#'   Default is Holdout.

# repls deleted - can be done by resampling?!

generateLearningCurve = function(learners, task, resampling = NULL,
  percs = seq(0.1, 1, by = 0.1), measures, stratify = FALSE)  {

  learners = lapply(learners, checkLearner)
  assertClass(task, "Task")
  assertNumeric(percs, lower = 0L, upper = 1L, min.len = 2L, any.missing = FALSE)
  measures = checkMeasures(measures, task)
  assertFlag(stratify)

  if (is.null(resampling))
    resampling = makeResampleInstance("Holdout", task = task)
  else
    assert(checkClasses(resampling, "ResampleDesc"), checkClasses(resampling, "ResampleInstance"))

  perc.ids = seq_along(percs)

  # create downsampled versions for all learners
  lrnds1 = lapply(learners, function(lrn) {
    lrn.downsampleds = lapply(perc.ids, function(p.id) {
      perc = percs[p.id]
      dsw = makeDownsampleWrapper(learner = lrn, dw.perc = perc, dw.stratify = stratify)
      list(
        lrn = setId(dsw, paste0(lrn$id, ".", p.id)),
        perc = perc
      )
    })
  })
  lrnds2 = unlist(lrnds1, recursive = FALSE)
  dsws = extractSubList(lrnds2, "lrn", simplify = FALSE)

  bench.res = benchmark(dsws, task, resampling,  measures)
  perfs = getBMRAggrPerformances(bench.res, as.df = TRUE)

  # get perc and learner col data
  perc = extractSubList(lrnds2[perfs$learner.id], "perc")
  learner = gsub("\\.\\d+$", "", perfs$learner.id)
  perfs = dropNamed(perfs, c("task.id", "learner.id"))

  # set short measures names and resort cols
  mids = extractSubList(measures, "id")
  colnames(perfs) = mids
  cbind(learner = learner, perc = perc, perfs)
}

plotLearningCurve = function(res) {
  library(ggplot2)
  ggdata = melt(res, id.vars = c("learner", "perc"), variable.name = "measure", value.name = "perf")
  ggplot(ggdata, aes_string(x = "perc", y = "perf", colour = "learner")) + layer(geom = "point") +
    layer(geom = "line") + facet_wrap(~measure, scales = "free_y")
}


r1 = generateLearningCurve(list("classif.rpart", "classif.knn") , task = iris.task, measures = list(mmce, timetrain))
print(plotLearningCurve(r1))

# r2 = generateLearningCurve(list("classif.rpart", "classif.knn", "classif.naiveBayes",
#     "classif.svm", "classif.randomForest"),
#   task = sonar.task, percs = seq(0.2, 1, by = 0.2),
#   measures = list(tp, fp, tn, fn), resampling = makeResampleDesc(method = "Subsample", iters = 6))
# print(plotLearningCurve(r2))

# r3 = generateLearningCurve(list("regr.ctree", "regr.lm", "regr.svm"), task = regr.num.task,
  # resampling = makeResampleDesc(method = "CV", iters = 5), measures = list(sse, timeboth))
# print(plotLearningCurve(r3))

