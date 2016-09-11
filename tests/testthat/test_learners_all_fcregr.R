context("learners_all_fcregr")

test_that("learners work: fcregr ", {

  # settings to make learners faster and deal with small data size
  hyperpars = list(
    fcregr.arfima = list(drange = c(0, .5), h = 101L),
    fcregr.Arima  = list(order = c(2,0,1), h = 101L),
    fcregr.ets    = list(model = "ANN", h = 101L),
    fcregr.bats   = list(use.box.cox = TRUE, h = 101L),
    fcregr.garch  = list(n.ahead = 101L),
    fcregr.nnetar = list(h = 101L),
    fcregr.tbats  = list(use.damped.trend = TRUE, h = 101L)
  )

  # Create smaller task:
  task = subsetTask(fcregr.task, subset = c(1:101))
  # normal fcregr
  lrns = mylist("fcregr", create = TRUE)
  lapply(lrns, testThatLearnerParamDefaultsAreInParamSet)
  lapply(lrns, testThatLearnerCanTrainPredict, task = task, hyperpars = hyperpars)

  # fcregr with quantile
  lrns = mylist(task, properties = "quantile", create = TRUE)
  lapply(lrns, testThatLearnerCanTrainPredict, task = task, hyperpars = hyperpars,
         pred.type = "quantile")

  # fcregr with weights
  lrns = mylist("fcregr", properties = "weights", create = TRUE)
  lapply(lrns, testThatLearnerRespectsWeights, hyperpars = hyperpars,
         task = task, train.inds = 1:101, test.inds = 1:101, weights = rep(c(1, 5), length.out = 101),
         pred.type = "response", get.pred.fun = getPredictionResponse)

  # fcregr with missing
  lrns = mylist("fcregr", properties = "missings", create = TRUE)
  lapply(lrns, testThatLearnerHandlesMissings, task = task, hyperpars = hyperpars)

  # fcregr variable importance
  lrns = mylist("fcregr", properties = "featimp", create = TRUE)
  lapply(lrns, testThatLearnerCanCalculateImportance, task = task, hyperpars = hyperpars)

})
