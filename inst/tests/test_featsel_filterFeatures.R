context("filterFeatures")

test_that("filterFeatures", {
  ns = getTaskFeatureNames(binaryclass.task)
  feat.imp = getFeatureFilterValues(binaryclass.df, binaryclass.target)
  expect_equal(names(feat.imp), ns)
  f = filterFeatures(binaryclass.df, target=binaryclass.target, threshold=-Inf)
  expect_equal(f, binaryclass.df)
	
  feat.imp = getFeatureFilterValues(obj=binaryclass.task, method="chi.squared")
  expect_equal(names(feat.imp), ns)
  f = filterFeatures(binaryclass.task, method="chi.squared", n = 5L)
  expect_equal(getTaskFeatureNames(f), sprintf("V%i", sort(tail(order(feat.imp), 5L))))
  
  ns = getTaskFeatureNames(multiclass.task)
  feat.imp = getFeatureFilterValues(multiclass.df, multiclass.target)
  expect_equal(names(feat.imp), ns)
  f1 = filterFeatures(obj = multiclass.task, n = round(0.5*ncol(multiclass.df)))
  f2 = filterFeatures(obj = multiclass.df, target = multiclass.target, percentage = 0.5)
  expect_equal(getTaskData(f1), f2)
})