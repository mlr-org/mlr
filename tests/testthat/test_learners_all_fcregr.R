context("learners_all_fcregr")

test_that("learners work: fcregr ", {

  # settings to make learners faster and deal with small data size
  hyperpars = list(
    fcregr.arfima        = list(drange = c(0, .5), h = 101L),
    fcregr.Arima         = list(order = c(2, 0, 1), h = 101L),
    fcregr.auto.arima    = list(h = 101L),
    fcregr.bats          = list(use.box.cox = TRUE, h = 101L),
    fcregr.ets           = list(model = "ANN", h = 101L),
    fcregr.garch         = list(n.ahead = 101L),
    fcregr.nnetar        = list(h = 101L, npaths = 10),
    fcregr.tbats         = list(use.damped.trend = TRUE, h = 101L)
  )

  # Create smaller task:
  task = subsetTask(fcregr.task, subset = c(1:101))
  # normal fcregr
  lrns = mylist("fcregr", create = TRUE)[6]
  lapply(lrns, testThatLearnerParamDefaultsAreInParamSet)
  lapply(lrns, testBasicLearnerProperties, task = task, hyperpars = hyperpars)

  # fcregr with quantile
  lrns = mylist(task, properties = "quantile", create = TRUE)
  lapply(lrns, testBasicLearnerProperties, task = task, hyperpars = hyperpars,
         pred.type = "quantile")



})
