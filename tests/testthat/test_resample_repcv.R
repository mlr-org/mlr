context("resample: repcv")

test_that("repcv instance works", {
  rin = makeResampleInstance(makeResampleDesc("RepCV", folds = 10, reps = 3), task = multiclass.task)

  iters = rin$desc$iters
  expect_equal(iters, 10*3)
  reps = rin$desc$reps
  expect_equal(reps, 3)

  for (j in 1:3) {
    bag = c()
    for (i in 1:10) {
      k = as.integer((j-1)*10L + i)
      i1 = rin$train.inds[[i]]
      i2 = rin$test.inds[[i]]
      expect_equal(length(unique(i1)), 135)
      expect_equal(length(unique(i2)), 15)
      bag = c(bag, i2)
    }
    expect_equal(sort(unique(bag)), 1:150)
  }
  # check that resampling is really stochastic
  if (interactive()) {
    rin1 = makeResampleInstance(makeResampleDesc("RepCV", folds = 2, reps = 2), size = 500)
    rin2 = makeResampleInstance(makeResampleDesc("RepCV", folds = 2, reps = 2), size = 500)
    expect_true(!all(sort(rin1$test.inds[[1]]) == sort(rin2$test.inds[[1]])))
  }
})

test_that("repcv resampling works", {
  rdesc = makeResampleDesc("RepCV", folds = 2, reps = 2)
  m = setAggregation(mmce, testgroup.mean)
  resample(makeLearner("classif.lda"), multiclass.task, rdesc)
})

