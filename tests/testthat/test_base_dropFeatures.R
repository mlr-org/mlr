context("dropFeatures")

test_that("dropFeatures", {
  fns = getTaskFeatureNames(multiclass.task)
  task2 = dropFeatures(multiclass.task, fns[1])
  expect_equal(length(getTaskFeatureNames(task2)), 3L)
})
