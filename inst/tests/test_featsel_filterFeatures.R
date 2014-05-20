context("filterFeatures")

test_that("filterFeatures", {
  ns = getTaskFeatureNames(binaryclass.task)
  feat.imp = getFeatureFilterValues(binaryclass.df, binaryclass.target)
  expect_equal(names(feat.imp), ns)
  f = filterFeatures(binaryclass.df, target=binaryclass.target,  feat.importance = feat.imp)
  expect_equal(f, binaryclass.df[, feat.imp > 0])
	
  feat.imp = getFeatureFilterValues(obj=binaryclass.task, method="chi.squared")
  expect_equal(names(feat.imp), ns)
  f = filterFeatures(binaryclass.task, feat.importance = feat.imp, n = 5L)
  expect_equal(getTaskFeatureNames(f), sprintf("V%i", sort(tail(order(feat.imp), 5L))))
  
  ns = getTaskFeatureNames(multiclass.task)
  feat.imp = getFeatureFilterValues(multiclass.df, multiclass.target)
  expect_equal(names(feat.imp), ns)
  f1 = filterFeatures(obj = multiclass.task, feat.importance = feat.imp, 
                      n = round(0.5*ncol(multiclass.df)))
  f2 = filterFeatures(obj = multiclass.df, target = multiclass.target, 
                      feat.importance = feat.imp, percentage = 0.5)
  expect_equal(getTaskData(f1), f2)
})