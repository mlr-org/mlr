context("FilterWrapper extra")

test_that("FilterWrapper extra", {
  lrn1 = makeLearner("classif.lda")
  lrn2 = makeFilterWrapper(lrn1, fw.method = "chi.squared", fw.perc = 1)
  m = train(lrn2, binaryclass.task)
  expect_true(!inherits(m, "FailureModel"))
  expect_equal(m$features, getTaskFeatureNames(binaryclass.task))
  lrn2 = makeFilterWrapper(lrn1, fw.method = "chi.squared", fw.abs = 0L)
  m = train(lrn2, binaryclass.task)
  expect_equal(getLeafModel(m)$features, character(0))
  expect_true(inherits(getLeafModel(m)$learner.model, "NoFeaturesModel"))
})
