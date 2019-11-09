context("resample_repcv")

test_that("repcv instance works", {
  rin = makeResampleInstance(makeResampleDesc("RepCV", folds = 10, reps = 3),
    task = multiclass.task)

  iters = rin$desc$iters
  expect_equal(iters, 10 * 3)
  reps = rin$desc$reps
  expect_equal(reps, 3)

  for (j in 1:3) {
    bag = NULL
    for (i in 1:10) {
      k = as.integer((j - 1) * 10L + i)
      i1 = rin$train.inds[[i]]
      i2 = rin$test.inds[[i]]
      expect_equal(length(unique(i1)), 135)
      expect_equal(length(unique(i2)), 15)
      bag = c(bag, i2)
    }
    expect_equal(sort(unique(bag)), 1:150)
  }
})

test_that("repcv resampling works", {
  rdesc = makeResampleDesc("RepCV", folds = 2, reps = 2)
  m = setAggregation(mmce, testgroup.mean)
  res = resample(makeLearner("classif.lda"), multiclass.task, rdesc)
  expect_class(res, "ResampleResult")
})

test_that("repcv instance is stochastic", {
  rin = makeResampleInstance(makeResampleDesc("RepCV", folds = 10, reps = 3),
    task = multiclass.task)

  iters = rin$desc$iters
  expect_equal(iters, 10 * 3)
  reps = rin$desc$reps
  expect_equal(reps, 3)

  for (j in 1:3) {
    bag = NULL
    for (i in 1:10) {
      k = as.integer((j - 1) * 10L + i)
      i1 = rin$train.inds[[i]]
      i2 = rin$test.inds[[i]]
      expect_equal(length(unique(i1)), 135)
      expect_equal(length(unique(i2)), 15)
      bag = c(bag, i2)
    }
    expect_equal(sort(unique(bag)), 1:150)
  }
  rin1 = makeResampleInstance(makeResampleDesc("RepCV", folds = 2, reps = 2), size = 500)
  rin2 = makeResampleInstance(makeResampleDesc("RepCV", folds = 2, reps = 2), size = 500)
  expect_true(!all(sort(rin1$test.inds[[1]]) == sort(rin2$test.inds[[1]])))
})

test_that("test.join works somehow", {
  df = data.frame(t = factor(c(rep(c("a", "b"), each = 3), "c", "c")), x = 1:8)
  task = makeClassifTask(data = df, target = "t")
  lrn = makeLearner("classif.rpart")
  measures = list(mmce, setAggregation(mmce, test.join))
  rin = makeResampleInstance(makeResampleDesc("RepCV", reps = 5, folds = 3), task = task)
  res = resample(learner = lrn, task = task, resampling = rin, measures = measures)
  expect_equal(res$measures.test[, 2L], res$measures.test[, 3L])
  expect_true(diff(res$aggr) > 0)

  lrn = setPredictType(lrn, predict.type = "prob")
  res.prob = resample(learner = lrn, task = task, resampling = rin, measures = measures)
  expect_equal(res.prob$measures.test[, 2L], res.prob$measures.test[, 3L])
})
