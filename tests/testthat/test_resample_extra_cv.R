context("resample: cv extra")


test_that("cv instance works is stochastic", {
  rin = makeResampleInstance(makeResampleDesc("CV", iters = 3), size = 25)

  folds = rin$desc$iters
  expect_equal(folds, 3)

  for (i in 1:folds) {
    i1 = rin$train.inds[[i]]
    i2 = rin$test.inds[[i]]
    expect_true(min(i1) >= 1)
    expect_true(max(i1) <= 25)
    expect_true(min(i2) >= 1)
    expect_true(max(i2) <= 25)
    expect_equal(sort(c(unique(i1), i2)), 1:25)
  }
  rin1 = makeResampleInstance(makeResampleDesc("CV", iters = 2L), size = 500)
  rin2 = makeResampleInstance(makeResampleDesc("CV", iters = 2L), size = 500)
  expect_true(!all(sort(rin1$test.inds[[1]]) == sort(rin2$test.inds[[1]])))
})
