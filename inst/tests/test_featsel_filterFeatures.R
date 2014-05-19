context("filterFeatures")

test_that("filterFeatures", {
  ns = getTaskFeatureNames(binaryclass.task)
  f = filter(binaryclass.df, target=binaryclass.target)
  expect_equal(f, binaryclass.df)
	f = filter(obj=binaryclass.task, method="chi.squared")
  expect_equal(getTaskFeatureNames(f), ns)
  f1 = filter(obj=multiclass.task, n=round(0.5*ncol(multiclass.df)))
  f2 = filter(obj=multiclass.df, target=multiclass.target, percentage=0.5)
  expect_equal(getTaskData(f1), f2)
})