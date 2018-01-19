context("getFeatureImportance")

test_that("getFeatureImportance", {

  #type 2 for random Forest should work without setting importance
  lrn = makeLearner("classif.randomForest")
  mod = train(lrn, binaryclass.task)
  feat.imp = getFeatureImportance(mod, type = 2)$res
  expect_data_frame(feat.imp, types = rep("numeric", getTaskNFeats(binaryclass.task)),
    any.missing = FALSE, nrows = 1, ncols = getTaskNFeats(binaryclass.task))
  expect_equal(colnames(feat.imp), mod$features)

  #type 1 shouldn't
  expect_error(getFeatureImportance(mod, type = 1), regexp = ".*importance.*TRUE")

  lrn = setHyperPars(lrn, importance = TRUE)
  mod = train(lrn, binaryclass.task)
  feat.imp = getFeatureImportance(mod, type = 1)$res
  expect_data_frame(feat.imp, types = rep("numeric", getTaskNFeats(binaryclass.task)),
    any.missing = FALSE, nrows = 1, ncols = getTaskNFeats(binaryclass.task))
  expect_equal(colnames(feat.imp), mod$features)

  #regression learner
  lrn = makeLearner("regr.gbm")
  mod = train(lrn, regr.task)
  feat.imp = getFeatureImportance(mod)$res
  expect_data_frame(feat.imp, types = rep("numeric", getTaskNFeats(regr.task)),
    any.missing = FALSE, nrows = 1, ncols = getTaskNFeats(regr.task))
  expect_equal(colnames(feat.imp), mod$features)

  #wrapped learner
  lrn = makeFilterWrapper(makeLearner("regr.gbm"), fw.method = "information.gain", fw.abs = 2)
  mod = train(lrn, regr.task)
  feat.imp = getFeatureImportance(mod)$res
  expect_data_frame(feat.imp, types = rep("numeric", getTaskNFeats(regr.task)),
    any.missing = FALSE, nrows = 1, ncols = getTaskNFeats(regr.task))
  expect_equal(colnames(feat.imp), mod$features)

  #For learners without the possibility to calculate feature importance a meaningfull error should
  #be returned
  lrn = makeLearner("classif.qda")
  mod = train(lrn, binaryclass.task)
  expect_error(getFeatureImportance(mod), regexp = "does not support 'featimp'")
})
