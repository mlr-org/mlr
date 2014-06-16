context("filterFeatures")

test_that("filterFeatures", {
  ns = getTaskFeatureNames(binaryclass.task)
  feat.imp = getFilterValues(binaryclass.task)
  expect_equal(feat.imp$data$name, ns)
  f = filterFeatures(binaryclass.task, select = "threshold", val = -Inf)
  expect_equal(f, binaryclass.task)

  feat.imp = getFilterValues(binaryclass.task, method = "chi.squared")
  expect_equal(feat.imp$data$name, ns)
  f = filterFeatures(binaryclass.task, method = "chi.squared", select = "abs", val = 5L)
  expect_equal(getTaskFeatureNames(f), head(sortByCol(feat.imp$data, "val", asc = FALSE), 5L)$name)

  f1 = filterFeatures(multiclass.task, select = "abs", val = round(0.5 * ncol(multiclass.df)))
  f2 = filterFeatures(multiclass.task, val = 0.5)
  expect_equal(f1, f2)
})
