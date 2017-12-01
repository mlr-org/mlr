context("checkTaskLearner")

test_that("checkTaskLearner", {
  df = multiclass.df
  df[1, 1] = NA
  task = makeClassifTask(data = df, target = multiclass.target)
  expect_error(train(makeLearner("classif.lda"), task), "missing values")
  expect_error(train(makeLearner("regr.km"), regr.task), "factor inputs")
  expect_error(train(makeLearner("classif.gbm"), regr.task), "is for 'classif'")
  expect_error(train(makeLearner("regr.gbm"), multiclass.task), "is for 'regr'")
})
