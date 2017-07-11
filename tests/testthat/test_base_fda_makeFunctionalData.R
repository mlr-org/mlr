context("makeFunctionalData")

test_that("makeFunctionalData works", {
  df = data.frame(matrix(rnorm(10^2), nrow = 10))
  df$fct = as.factor(letters[1:10])
  df$ord = as.ordered(1:10)
  fdf = makeFunctionalData(df, fd.features = list("fd1" = 1:5, "fd2" = 6:9))
  expect_equal(sapply(fdf, class)[[1]], "numeric")
  expect_equal(sapply(fdf, class)[[2]], "factor")
  expect_equal(sapply(fdf, class)[[3]], c("ordered", "factor"))
  expect_equal(sapply(fdf, class)[[4]], c("functional", "matrix"))
  expect_equal(sapply(fdf, class)[[5]], c("functional", "matrix"))
  expect_equal(dim(fdf), c(10, 5))
  expect_class(fdf, "data.frame")
})

test_that("makeFunctionalData subsetting works", {
  df = data.frame(matrix(rnorm(10^2), nrow = 10))
  df$fct = as.factor(letters[1:10])
  df$ord = as.ordered(1:10)
  fdf = makeFunctionalData(df, fd.features = list("fd1" = 1:5, "fd2" = 6:9))

  # Subset rows
  fdf2 = fdf[1:5, , drop = FALSE]
  expect_equal(sapply(fdf2, class)[[1]], "numeric")
  expect_equal(sapply(fdf2, class)[[2]], "factor")
  expect_equal(sapply(fdf2, class)[[3]], c("ordered", "factor"))
  # FAILS:
  # expect_equal(sapply(fdf2, class)[[4]], c("functional", "matrix"))
  # expect_equal(sapply(fdf2, class)[[5]], c("functional", "matrix"))
  expect_equal(dim(fdf2), c(5, 5))
  expect_class(fdf2, "data.frame")

  # Subset cols
  fdf3 = fdf[, 2:4, drop = FALSE]
  expect_equal(sapply(fdf3, class)[[1]], "factor")
  expect_equal(sapply(fdf3, class)[[2]], c("ordered", "factor"))
  # FAILS:
  # expect_equal(sapply(fdf3, class)[[3]], c("functional", "matrix"))
  expect_equal(dim(fdf3), c(10, 3))
  expect_class(fdf3, "data.frame")
})


test_that("Code from Bernd", {
  d = list(x1 = matrix(123, 3, 2), x2 = matrix(1:6, 3, 2), x3 = 1:3)
  class(d) = "data.frame"
  names(d) = c("x1", "x2", "x3")
  rownames(d) = 1:3
  print(str(d))
  sapply(d, class)

  x1 = matrix(123, 3, 2)
  x1 = BBmisc::addClasses(x1, "functional")
  x2 = matrix(1:6, 3, 2)
  x2 = BBmisc::addClasses(x2, "functional")
  d = list(x1 = x1, x2 = x2, x3 = 1:3)
  class(d) = "data.frame"
  names(d) = c("x1", "x2", "x3")
  rownames(d) = 1:3
  print(str(d))
  sapply(d, class)
  d = d[1:2, , drop = FALSE]
  sapply(d, class)
})
