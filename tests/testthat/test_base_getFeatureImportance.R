context("getFeatureImportance")

test_that("getFeatureImportance", {

  # type 2 for random Forest should work without setting importance
  lrn = makeLearner("classif.randomForest")
  mod = train(lrn, binaryclass.task)
  feat.imp = getFeatureImportance(mod, type = 2)$res
  expect_data_frame(feat.imp, types = c("character", "numeric"),
    any.missing = FALSE, nrows = 60, ncols = 2)
  expect_equal(colnames(feat.imp), c("variable", "importance"))

  # type 1 shouldn't
  expect_error(getFeatureImportance(mod, type = 1), regexp = ".*importance.*TRUE")

  lrn = setHyperPars(lrn, importance = TRUE)
  mod = train(lrn, binaryclass.task)
  feat.imp = getFeatureImportance(mod, type = 1)$res
  expect_data_frame(feat.imp, types = c("character", "numeric"),
    any.missing = FALSE, nrows = 60, ncols = 2)
  expect_equal(colnames(feat.imp), c("variable", "importance"))

  # regression learner
  lrn = makeLearner("regr.gbm")
  mod = train(lrn, regr.task)
  feat.imp = getFeatureImportance(mod)$res
  expect_data_frame(feat.imp, types = c("character", "numeric"),
    any.missing = FALSE, nrows = 13, ncols = 2)
  expect_equal(colnames(feat.imp), c("variable", "importance"))

  # wrapped learner
  lrn = makeFilterWrapper(makeLearner("regr.gbm"),
    fw.method = "FSelectorRcpp_information.gain", fw.abs = 2, equal = TRUE)
  mod = train(lrn, regr.task)
  feat.imp = getFeatureImportance(mod)$res
  expect_data_frame(feat.imp, types = c("character", "numeric"),
    any.missing = FALSE, nrows = 13, ncols = 2)
  expect_equal(colnames(feat.imp), c("variable", "importance"))

  # For learners without the possibility to calculate feature importance a
  # meaningful error should be returned
  lrn = makeLearner("classif.qda")
  mod = train(lrn, binaryclass.task)
  expect_error(getFeatureImportance(mod), regexp = "does not support 'featimp'")
})
