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

generateLearningCurve = function(lrns, task, resampling = NULL,
  percs = seq(0.1, 1, by = 0.1), measures, stratify = FALSE)  {

  assertClass(task, "Task")
  assertNumeric(percs, lower = 0L, upper = 1L, min.len = 2L, any.missing = FALSE)
  
  if (is.null(resampling)) {
    resampling = makeResampleInstance("Holdout", task = task)
  } else if (inherits(resampling, "ResampleDesc")) { #maybe not necessary because in benchmark
    resampling = makeResampleInstance(resampling, task = task)
  }
  
  lrns = lapply(lrns, checkLearner)
  measures = checkMeasures(measures, task)
  perc.ids = seq_along(percs)

  all.learners = lapply(lrns, function(lrn) {
    lrn.downsampleds = lapply(perc.ids, function(p.id){
      lrn.downsampled = makeDownsampleWrapper(learner = lrn, dw.perc = percs[p.id], dw.stratify = stratify)
      lrn.downsampled$id = paste0(lrn.downsampled$id, ".", p.id)
      lrn.downsampled
    })
  })
  bench.res = benchmark(learners = unlist(all.learners, recursive = FALSE), task = task, resamplings = resampling, measures = measures)
  
  perfs = getBMRAggrPerformances(bench.res)[[task$task.desc$id]]
  res = expand.grid(dw.perc.id = perc.ids, learner = extractSubList(lrns, "id"))
  res = lapply(seq_row(res), function(i) {
    #m = perfs[[paste0(res[i,"learner"], ".downsampled.", res[i, "dw.perc.id"]]] #FIXME: method a ...
    m = perfs[[i]] #or method b ?
    data.frame(dw.perc = percs[res[i, "dw.perc.id"]], learner = res[i,"learner"], performance = m, measure = names(m))
    })
  res = do.call(rbind, res)
  rownames(res) = NULL
  res
}

plotLearningCurve = function(res) {
  library(ggplot2)
  ggplot(res, aes(x = dw.perc, y = performance, colour = learner)) + layer(geom = "point") +
    layer(geom = "line") + facet_wrap(~measure, scales = "free_y")
}


r1 = generateLearningCurve(lrns = list("classif.rpart", "classif.knn", "classif.svm"), task = iris.task, measures = list(mmce, timetrain))
plotLearningCurve(r1)

r2 = generateLearningCurve(lrns = list("classif.rpart", "classif.knn", "classif.naiveBayes",
                                       "classif.svm", "classif.randomForest"),
                            task = sonar.task, percs = seq(0.2, 1, by = 0.2),
                            measures = list(tp, fp, tn, fn), resampling = makeResampleDesc(method = "Subsample", iters = 6))
plotLearningCurve(r2)

r3 = generateLearningCurve(lrns = list("regr.ctree", "regr.lm", "regr.svm"), task = regr.num.task, resampling = makeResampleDesc(method = "CV", iters = 5), measures = list(sse, timeboth))
plotLearningCurve(r3)

