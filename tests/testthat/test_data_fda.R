context("fda dataframe")
test_that("makeFDAData", {
  fda.name = "functional"
  fd1 = makeFunctionalFeature(matrix(rnorm(100), ncol = 20))
  expect_true(inherits(fd1, fda.name))
})

