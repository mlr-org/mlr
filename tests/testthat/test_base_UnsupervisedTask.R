context("UnsupervisedTask")

test_that("UnsupervisedTask", {
  ct1 = noclass.task

  expect_error(makeClassifTask(data = 44), "'data.frame'")

  # wrong vars
  expect_error(subsetTask(noclass.task, vars = c("Sepal.Length", "x", "y")))

  # check missing accessors
  df = noclass.df
  df[1,1:3] = NA
  df[2,1:3] = NA
  ct = makeClusterTask(data = df)
  td = getTaskDescription(ct)
  expect_true(td$has.missings)

  # check that blocking is still there after subsetting
  ct1 = makeClusterTask(data = noclass.df, blocking = as.factor(1:nrow(noclass.df)))
  expect_true(getTaskDescription(ct1)$has.blocking)
  ct2 = subsetTask(ct1)
  expect_true(getTaskDescription(ct2)$has.blocking)
})
