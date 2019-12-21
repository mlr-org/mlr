context("learners_all_multilabel")

test_that("learners work: multilabel", {

  # settings to make learners faster and deal with small sample size
  hyperpars = list()

  # multiabel
  lrns = listLearnersCustom("multilabel", create = TRUE)
  lapply(lrns, testThatLearnerParamDefaultsAreInParamSet)
  lapply(lrns, testBasicLearnerProperties, task = multilabel.task,
    hyperpars = hyperpars)

  # multilabel, probs
  lrns = listLearnersCustom("multilabel", properties = "prob", create = TRUE)
  lapply(lrns, testBasicLearnerProperties, task = multilabel.task,
    hyperpars = hyperpars, pred.type = "prob")

  # multilabel, factors
  lrns = listLearnersCustom("multilabel", properties = "factors", create = TRUE)
  lapply(lrns, testThatLearnerHandlesFactors, task = multilabel.task,
    hyperpars = hyperpars)

  # multilabel, ordered
  lrns = listLearnersCustom("multilabel", properties = "ordered", create = TRUE)
  lapply(lrns, testThatLearnerHandlesOrderedFactors, task = multilabel.task,
    hyperpars = hyperpars)

  # multilabel, missings
  lrns = listLearnersCustom("multilabel", properties = "missings", create = TRUE)
  lapply(lrns, testThatLearnerHandlesMissings, task = multilabel.task,
    hyperpars = hyperpars)

  # multilabel, weights
  lrns = listLearnersCustom("multilabel", properties = "weights", create = TRUE)
  lapply(lrns, testThatLearnerRespectsWeights, hyperpars = hyperpars,
    task = multilabel.task, train.inds = multilabel.train.inds, multilabel.test.inds,
    weights = rep(c(10000L, 1L), c(10L, length(multilabel.train.inds) - 10L)),
    pred.type = "prob", get.pred.fun = getPredictionProbabilities)
})
