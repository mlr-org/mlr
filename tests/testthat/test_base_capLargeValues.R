context("capLargeValues")

test_that("capLargeValues", {
  d1 = data.frame(x = 1:10, y = c(1:9, Inf), z = c(-11:-20))
  d2 = capLargeValues(d1, threshold = 10, impute = 10)
  expect_equal(d2, data.frame(x = 1:10, y = c(1:10), z = rep(-10,10)))
  d2 = capLargeValues(d1, threshold = 50, impute = 10, cols = "y")
  expect_equal(d2, data.frame(x = 1:10, y = c(1:9, 50), z = c(-11:-20)))
  d2 = capLargeValues(d1, threshold = -100, impute = -2, cols = 3)
  expect_equal(d2, data.frame(x = 1:10, y = c(1:9, Inf), z = rep(-2, 10)))

  d1 = data.frame(x = c(-10, 1, 10))
  d2 = capLargeValues(d1, threshold = 9, what = "abs")
  expect_equal(d2, data.frame(x = c(-9, 1, 9)))
  d2 = capLargeValues(d1, threshold = 9, what = "high")
  expect_equal(d2, data.frame(x = c(-10, 1, 9)))
  d2 = capLargeValues(d1, threshold = 9, what = "low")
  expect_equal(d2, data.frame(x = c(-9, 1, 10)))
})
