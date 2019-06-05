context("resample_fixedwindowcv")

test_that("fixed instance works", {
  rin = makeResampleInstance(makeResampleDesc("FixedWindowCV"), size = 25)

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

test_that("fixed instance works with value < 1", {
  rin = makeResampleInstance(makeResampleDesc("FixedWindowCV", horizon = .1,
    initial.window = .5, skip = .02), size = 25)

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

test_that("fixed instance works with values > 1", {
  rin = makeResampleInstance(makeResampleDesc("FixedWindowCV", horizon = 2,
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

test_that("fixed instance throws warning for improper alignment", {
  expect_warning(makeResampleInstance(makeResampleDesc("FixedWindowCV",
    horizon = 2, initial.window = 8,
    skip = 2), size = 25))
})
