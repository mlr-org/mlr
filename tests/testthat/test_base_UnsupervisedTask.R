context("UnsupervisedTask")

test_that("UnsupervisedTask", {
  ct1 = noclass.task

  expect_error(makeClassifTask(data = 44), "'data.frame'")

  # wrong vars
  expect_error(subsetTask(noclass.task, features = c("Sepal.Length", "x", "y")))

  # subsetTask works with index vector
  sub.task = subsetTask(noclass.task, features = 1:2)
  expect_equal(sum(getTaskNFeats(sub.task)), 2L)

  # check missing accessors
  df = noclass.df
  df[1, 1:3] = NA
  df[2, 1:3] = NA
  ct = makeClusterTask(data = df)
  td = getTaskDesc(ct)
  expect_true(td$has.missings)

  # check that blocking is still there after subsetting
  ct1 = makeClusterTask(data = noclass.df, blocking = as.factor(seq_len(nrow(noclass.df))))
  expect_true(getTaskDesc(ct1)$has.blocking)
  ct2 = subsetTask(ct1, features = getTaskFeatureNames(ct1))
  expect_true(getTaskDesc(ct2)$has.blocking)

  # check 'fixup data' works

  expect_warning(makeClusterTask("cluster", iris[1:10, ], fixup.data = "warn"), "Empty factor levels")

  expect_warning(makeClusterTask("cluster", iris[1:10, ], fixup.data = "quiet"), NA)
})
