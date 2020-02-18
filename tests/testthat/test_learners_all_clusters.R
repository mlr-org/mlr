context("learners_all_clusters")

test_that("learners work: cluster", {
  requirePackagesOrSkip("clusterSim", default.method = "load")
  skip_on_os("windows")

  # RWeka not avail on CRAN
  skip_on_cran()

  # settings to make learners faster and deal with small sample size
  hyperpars = list()

  # clustering, response
  task = noclass.task
  lrns = listLearnersCustom(task, create = TRUE)
  # some learners are not avail on windows
  if (Sys.info()[["sysname"]] == "Windows") {
    names = vapply(lrns, function(x) x$id, FUN.VALUE = character(1))
    row_ids = which(names %in% "cluster.Cobweb")
    lrns[row_ids] = NULL
  }
  lapply(lrns, testThatLearnerParamDefaultsAreInParamSet)
  lapply(lrns, testBasicLearnerProperties, task = task, hyperpars = hyperpars)

  # clustering, prob
  task = subsetTask(noclass.task, subset = 1:20,
    features = getTaskFeatureNames(noclass.task)[1:2])
  lrns = listLearnersCustom(task, properties = "prob", create = TRUE)
  lapply(lrns, testBasicLearnerProperties, task = task, hyperpars = hyperpars,
    pred.type = "prob")

  # cluster with weights
  lrns = listLearnersCustom("cluster", properties = "weights", create = TRUE)
  lapply(lrns, testThatLearnerRespectsWeights, hyperpars = hyperpars,
    task = task, train.inds = 1:20, test.inds = 1:20, weights = rep(c(1, 5),
      length.out = 20),
    pred.type = "prob", get.pred.fun = getPredictionProbabilities)
})
