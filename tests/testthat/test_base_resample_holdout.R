context("resample_holdout")

test_that("holdout instance works", {
  rin = makeResampleInstance(makeResampleDesc("Holdout", split = 0.25), size = 20)
  expect_equal(rin$size, 20)
  expect_equal(rin$desc$iters, 1)
  expect_equal(length(rin$train.inds), 1)
  expect_equal(length(rin$test.inds), 1)
  expect_equal(length(rin$train.inds[[1]]), 5)
  expect_equal(length(rin$test.inds[[1]]), 15)
})

test_that("holdout fixed instance works", {
  rin = makeFixedHoldoutInstance(1:5, 1:15, size = 20)
  expect_equal(rin$size, 20)
  expect_equal(rin$desc$iters, 1)
  expect_equal(length(rin$train.inds), 1)
  expect_equal(length(rin$test.inds), 1)
  expect_equal(length(rin$train.inds[[1]]), 5)
  expect_equal(length(rin$test.inds[[1]]), 15)
})

test_that("holdout test.join works somehow", {
  lrn = makeLearner("classif.rpart", predict.type = "prob")

  # for holdout test.join and test.mean should be the same
  rin = makeResampleDesc("Holdout")
  mm = list(setAggregation(auc, test.join), auc)
  r = resample(lrn, sonar.task, rin, measures = mm)
  expect_equal(as.integer(diff(r$aggr)), 0)
})
