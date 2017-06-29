context("fda dataframe")
test_that("makeFDAData", {
  FDA_DATA_NAME = "fda.data.frame"
  df = iris
  dfn = makeFDAData(df, fd.features = list(fd1 = 1:2, fd2 = 3:4), fd.grids = NULL)
  expect_true(inherits(dfn, FDA_DATA_NAME))
})

