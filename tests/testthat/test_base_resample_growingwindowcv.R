context("resample_growingwindowcv")

test_that("growing window instance works", {
  rin = makeResampleInstance(desc = makeResampleDesc(method = "GrowingWindowCV"), size = 25)

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

test_that("growing instance with values > 1 works", {
  rin = makeResampleInstance(desc = makeResampleDesc(method = "GrowingWindowCV", horizon = 2,
    initial.window = 8, skip = 1), size = 25)

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

test_that("growing window instance with values < 1 works", {
  rin = makeResampleInstance(desc = makeResampleDesc(method = "GrowingWindowCV", horizon = .1,
    initial.window = .3, skip = .02), size = 25)

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

test_that("growing window instance throws warning for improper alignment", {
  expect_warning(makeResampleInstance(makeResampleDesc("GrowingWindowCV",
    horizon = 2, initial.window = 8,
    skip = 2), size = 25))
})
