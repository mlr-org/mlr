context("resample_growingcv")


test_that("growing instance works", {
  rin = makeResampleInstance(makeResampleDesc("GrowingCV", horizon = 2), size = 25)

  for (i in seq_len(length(rin$train.inds))) {
    i1 = rin$train.inds[[i]]
    i2 = rin$test.inds[[i]]
    expect_true(min(i1) >= 1)
    expect_true(max(i1) <= 25)
    expect_true(min(i2) >= 1)
    expect_true(max(i2) <= 25)
    expect_true(max(i1) < min(i2))
  }
})


