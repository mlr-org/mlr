context("checkTaskSubset")

test_that("checkTaskSubset", {
  expect_equal(1:50, checkTaskSubset(1:50, size = 50))
  subs20 = sample.int(50, 20)
  expect_equal(subs20, checkTaskSubset(subs20, size = 50))

  subs.bool = sample(c(TRUE, FALSE), size = 50, replace = TRUE)
  expect_equal(which(subs.bool), checkTaskSubset(subs.bool, size = 50))
  expect_error(checkTaskSubset(subs20, size = 10), regexp = "<= 10")

  # oversampling is allowed
  subs50 = sample.int(20, 50, replace = TRUE)
  expect_equal(subs50, checkTaskSubset(subs50, size = 20))
})
