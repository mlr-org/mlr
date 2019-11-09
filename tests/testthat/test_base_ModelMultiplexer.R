context("ModelMultiplexer")

test_that("ModelMultiplexer inherits properties", {
  bls = list(
    makeLearner("classif.lda"),
    makeLearner("classif.randomForest")
  )
  lrn = makeModelMultiplexer(bls)
  expect_set_equal(getLearnerProperties(lrn), getLearnerProperties(bls[[1L]]))

  lrn = setHyperPars(lrn, selected.learner = "classif.randomForest")
  expect_set_equal(getLearnerProperties(lrn), getLearnerProperties(bls[[2L]]))
})
