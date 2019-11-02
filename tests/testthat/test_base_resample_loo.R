context("resample_loo")

test_that("loo instance works", {
  rin = makeResampleInstance(makeResampleDesc("LOO"), size = 10)

  iters = rin$desc$iters
  expect_equal(iters, 10)

  for (i in 1:iters) {
    i1 = rin$train.inds[[i]]
    i2 = rin$test.inds[[i]]
    expect_true(min(i1) >= 1)
    expect_true(max(i1) <= 10)
    expect_true(min(i2) >= 1)
    expect_true(max(i2) <= 10)
    expect_equal(length(i1), 9)
    expect_equal(length(i2), 1)
    expect_equal(sort(c(unique(i1), i2)), 1:10)
  }
})
