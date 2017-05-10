context("learners_all_oneclass")

test_that("learners work: oneclass ", {

  # settings to make learners faster and deal with small data size
  hyperpars = list()

  # oneclass
  lrns = mylist("oneclass", create = TRUE)
  lapply(lrns, testThatLearnerParamDefaultsAreInParamSet)
  lapply(lrns, testBasicLearnerProperties, task = oneclass.task, hyperpars = hyperpars)

  # oneclass with factors
  lrns = mylist("oneclass", properties = "factors", create = TRUE)
  lapply(lrns, testThatLearnerHandlesFactors, task = oneclass.task, hyperpars = hyperpars)


  # oneclass with prob
  # lrns = mylist("oneclass", properties = "prob", create = TRUE)
  # lapply(lrns, testBasicLearnerProperties, task = oneclass.task,
  #   hyperpars = hyperpars, pred.type = "prob")

  # oneclass with weights
  # lrns = mylist("oneclass", properties = "weights", create = TRUE)
  # lapply(lrns, testThatLearnerRespectsWeights, hyperpars = hyperpars,
  #   task = oneclass.task, train.inds = oneclass.train.inds, test.inds = oneclass.test.inds,
  #   weights = rep(c(10000L, 1L), c(10L, length(oneclass.train.inds) - 10L)),
  #   pred.type = "prob", get.pred.fun = getPredictionProbabilities)
})



