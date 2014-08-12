context("summarizeColumns")

test_that("summarizeColumns", {
  d = data.frame(x = 1:5, y = c("a","b", "c", "d", "e"),
   z = c(TRUE, TRUE, TRUE, FALSE, FALSE), stringsAsFactors = FALSE)

  s = summarizeColumns(d)
  expect_equal(dim(s), c(ncol(d), 9L))
  expect_equal(s$na, c(0, 0, 0))
  expect_equal(s$mean, c(3, NA, NA))

  s = summarizeColumns(iris.task)

  d = iris
  d[1L, 1L] = NA_real_
  s = summarizeColumns(d)
  expect_equal(s$na, c(1, 0, 0, 0, 0))

})

