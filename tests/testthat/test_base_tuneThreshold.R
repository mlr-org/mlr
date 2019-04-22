context("tuneThreshold")

test_that("tuneThreshold", {
  # binary classification
  res = makeResampleDesc("Holdout")
  lrn = makeLearner("classif.rpart", predict.type = "prob")
  rf = resample(lrn, task = binaryclass.task, resampling = res, measures = list(mmce))
  th = tuneThreshold(rf$pred, measure = mmce)

  expect_equal(length(th$th), 1L) # threshold for positive class
  expect_equal(length(th$perf), 1L) # 1d-performance value

  # multiclass classification
  rf2 = resample(lrn, task = multiclass.task, resampling = res, measures = list(mmce))
  th2 = tuneThreshold(rf2$pred, measure = mmce, control = list(max.call = 5))

  expect_equal(length(th2$perf), 1L) # 1d-performance value
  expect_equal(length(th2$th), length(getTaskClassLevels(multiclass.task))) # no. of threshold = no. of classes
  expect_equal(names(th2$th), getTaskClassLevels(multiclass.task)) # threshold names = class names
  expect_equal(sum(th2$th), 1L) # sum of thresholds = 1L
})
