context("summarizeColumns")

test_that("summarizeColumns", {
  d = data.frame(x = 1:5, y = c("a", "b", "c", "d", "e"),
    z = c(TRUE, TRUE, TRUE, FALSE, FALSE), stringsAsFactors = FALSE)

  s = summarizeColumns(d)
  expect_equal(dim(s), c(ncol(d), 10L))
  expect_equal(s$na, c(0, 0, 0))
  expect_equal(s$mean, c(3, NA, NA))

  s = summarizeColumns(iris.task)

  d = iris
  d[1L, 1L] = NA_real_
  s = summarizeColumns(d)
  expect_equal(s$na, c(1, 0, 0, 0, 0))
  expect_false(is.na(s[1L, "mean"]))
  expect_false(is.na(s[1L, "disp"]))
  expect_false(is.na(s[1L, "mad"]))
  expect_false(is.na(s[1L, "median"]))
  expect_equal(s[5L, "min"], 50L)
  expect_equal(s[5L, "max"], 50L)
  d = iris
  d[1L, 5L] = NA_real_
  s = summarizeColumns(d)
  expect_equal(s$na, c(0, 0, 0, 0, 1))
  expect_equal(s[5L, "min"], 49L)
  expect_equal(s[5L, "max"], 50L)
})
