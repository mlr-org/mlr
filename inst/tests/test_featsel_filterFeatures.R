context("filterFeatures")

test_that("filterFeatures", {
  ns = getTaskFeatureNames(binaryclass.task)
  feat.imp = getFilterValues(binaryclass.task)
  expect_equal(names(feat.imp), ns)
  f = filterFeatures(binaryclass.task, select = "threshold", val = -Inf)
  expect_equal(f, binaryclass.task)

  feat.imp = getFilterValues(binaryclass.task, method = "chi.squared")
  expect_equal(names(feat.imp), ns)
  f = filterFeatures(binaryclass.task, method = "chi.squared", select = "abs", val = 5L)
  expect_equal(getTaskFeatureNames(f), names(head(sort(feat.imp, decreasing = TRUE), 5L)))

  f1 = filterFeatures(multiclass.task, select = "abs", val = round(0.5 * ncol(multiclass.df)))
  f2 = filterFeatures(multiclass.task, val = 0.5)
  expect_equal(f1, f2)
})
