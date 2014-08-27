context("resample: bs extra")

test_that("bs instance is stochastic", {
  rin = makeResampleInstance(makeResampleDesc("Bootstrap", iters = 3), size = 25)

  iters = rin$desc$iters
  expect_equal(iters, 3)

  for (i in 1:iters) {
    i1 = rin$train.inds[[i]]
    i2 = rin$test.inds[[i]]
    expect_equal(length(i1), 25)
    expect_equal(length(i2), 25 - length(unique(i1)))
    expect_true(min(i1) >= 1)
    expect_true(max(i1) <= 25)
    expect_true(min(i2) >= 1)
    expect_true(max(i2) <= 25)
    expect_equal(sort(c(unique(i1), i2)), 1:25)
  }
  rin1 = makeResampleInstance(makeResampleDesc("Bootstrap", iters = 3), size = 500)
  rin2 = makeResampleInstance(makeResampleDesc("Bootstrap", iters = 3), size = 500)
  expect_true(!all(sort(rin1$train.inds[[1]]) = = sort(rin2$train.inds[[1]])))
})
