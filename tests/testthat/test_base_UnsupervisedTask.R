context("UnsupervisedTask")

test_that("UnsupervisedTask", {
  ct1 = noclass.task

  expect_error(makeClassifTask(data = 44),
    "Must be a data.frame")

  # wrong vars
  expect_error(subsetTask(noclass.task, vars = c("Sepal.Length", "x", "y")))

  # check missing accessors
  df = noclass.df
  df[1,1:3] = NA
  df[2,1:3] = NA
  ct = makeClusterTask(data = df)
  expect_true(ct$task.desc$has.missings)

  # check that blocking is still there after subsetting
  ct1 = makeClusterTask(data = noclass.df, blocking = as.factor(1:nrow(noclass.df)))
  expect_true(ct1$task.desc$has.blocking)
  ct2 = subsetTask(ct1)
  expect_true(ct2$task.desc$has.blocking)
})
