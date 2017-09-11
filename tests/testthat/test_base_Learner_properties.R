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

test_that("listLearnerProperties", {
  regr = c("numerics", "factors", "ordered", "missings", "weights", "se",
    "featimp", "oobpreds", "functionals", "single.functional")
  expect_equal(listLearnerProperties("regr"), regr)
  classif = c("numerics", "factors", "ordered", "missings", "weights", "prob",
    "oneclass", "twoclass", "multiclass", "class.weights", "featimp", "oobpreds",
    "functionals", "single.functional")
  expect_equal(listLearnerProperties("classif"), classif)
  surv = c("numerics", "factors", "ordered", "missings", "weights", "prob", "lcens",
    "rcens", "icens", "featimp", "oobpreds", "functionals", "single.functional")
  expect_equal(listLearnerProperties("surv"), surv)
  cluster = c("numerics", "factors", "ordered", "missings", "weights", "prob",
    "functionals", "single.functional")
  expect_equal(listLearnerProperties("cluster"), cluster)
})
