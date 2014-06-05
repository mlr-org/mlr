context("filterFeatures")

test_that("filterFeatures", {
  ns = getTaskFeatureNames(binaryclass.task)
  feat.imp = getFilterValues(binaryclass.df, binaryclass.target)
  expect_equal(names(feat.imp), ns)
  f = filterFeatures(binaryclass.df, target = binaryclass.target, select = "threshold", val = -Inf)
  expect_equal(f, binaryclass.df)

  feat.imp = getFilterValues(obj = binaryclass.task, method = "chi.squared")
  expect_equal(names(feat.imp), ns)
  f = filterFeatures(binaryclass.task, method = "chi.squared", select = "abs", val = 5L)
  expect_equal(getTaskFeatureNames(f), sprintf("V%i", sort(tail(order(feat.imp), 5L))))

  ns = getTaskFeatureNames(multiclass.task)
  feat.imp = getFilterValues(multiclass.df, multiclass.target)
  expect_equal(names(feat.imp), ns)
  f1 = filterFeatures(obj = multiclass.task, select = "abs", val = round(0.5 * ncol(multiclass.df)))
  f2 = filterFeatures(obj = multiclass.df, target = multiclass.target, val = 0.5)
  expect_equal(getTaskData(f1), f2)
})
