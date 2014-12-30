
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
  } else if (inherits(resampling, "ResampleDesc")) {
    resampling = makeResampleInstance(resampling, task = task)
  }
  
  lrns = lapply(lrns, checkLearner)
  measures = checkMeasures(measures, task)

  all.learners = lapply(lrns, function(lrn) {
    lrn.downsampleds = lapply(percs, function(p){
      lrn.downsampled = makeDownsampleWrapper(learner = lrn, dw.perc = p, dw.stratify = stratify)
      lrn.downsampled$id = paste0(lrn.downsampled$id, ".", as.character(p))
      lrn.downsampled
    })
  })
  all.learners = unlist(all.learners, recursive = FALSE)
  bench.res = benchmark(learners = all.learners, task = task, resamplings = resampling, measures = measures)
  perfs = getBMRAggrPerformances(bench.res)[[task$task.desc$id]]
  # continue here
  
  res = data.frame()
  for (j in 1:l) {
    for (i in 1:m) {
      new = data.frame(n.obs = n.obs, perfs = colMeans(perfs[[j]])[,i],
                       measure = measure.names[i], learner = lrn.names[j])
      res = rbind(res, new)
    }
  }
  rownames(res) = NULL
  
  return(res)
}

plotLearningCurve = function(res) {
  library(ggplot2)
  ggplot(res, aes(x = n.obs, y = perfs, colour = learner)) + layer(geom = "point") +
    layer(geom = "line") + facet_wrap(~measure)
}


r1 = generateLearningCurve(lrns = list("classif.rpart", "classif.knn", "classif.svm"), task = iris.task, measures = list(mmce, acc))
plotLearningCurve(r1)

# r2 = generateLearningCurve(lrns = list("classif.rpart", "classif.knn", "classif.naiveBayes",
#                                       "classif.svm", "classif.plr", "classif.randomForest"),
#                            task = sonar.task, test.size = 0.25, percs = seq(0.2, 1, by = 0.2),
#                            measures = list(tp, fp, tn, fn), repls = 6L)
# plotLearningCurve(r2)

