context("Learner_properties")

test_that("Learner_properties", {
  lrn.char = "regr.glm"
  lrn = makeLearner(lrn.char)
  lrn.prop = getLearnerProperties(lrn)
  lrn.char.prop = getLearnerProperties(lrn.char)
  expect_equal(lrn.prop, lrn.char.prop)
  expect_equal(lrn.prop, c("numerics", "factors", "se", "weights"))

  lrn.wrap = makeImputeWrapper(lrn)
  expect_equal(getLearnerProperties(lrn.wrap), c(lrn.prop, "missings"))
})
