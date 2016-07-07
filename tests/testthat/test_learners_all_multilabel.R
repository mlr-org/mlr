context("learners_all_multilabel")

test_that("learners work: multilabel", {

  # settings to make learners faster and deal with small sample size
hyperpars = list()

# multiabel
lrns = mylist("multilabel", create = TRUE)
lapply(lrns, testThatLearnerCanTrainPredict, task = multilabel.task, hyperpars = hyperpars)

# multilabel, factors
lrns = mylist("multilabel", properties = "factors", create = TRUE)
lapply(lrns, testThatLearnerHandlesFactors, task = multilabel.task, hyperpars = hyperpars)

})