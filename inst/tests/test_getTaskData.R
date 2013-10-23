context("getTaskData")

test_that("getTaskData", {
  df = getTaskData(multiclass.task)
  expect_equal(df, multiclass.df)
  df = getTaskData(multiclass.task, subset=1:10, features=colnames(multiclass.df)[1:2])
  expect_equal(df, multiclass.df[1:10, c(1:2, 5)])

  # recode.target
  df = getTaskData(binaryclass.task, recode.target="01")
  expect_equal(df[, 1:20], binaryclass.df[, 1:20])
  expect_true(is.numeric(df[, binaryclass.target]))
  expect_equal(sum(df[, binaryclass.target] == 1), 
    sum(binaryclass.df[, binaryclass.target] == binaryclass.task$task.desc$positive))
  expect_equal(sum(df[, binaryclass.target] == 0), 
    sum(binaryclass.df[, binaryclass.target] == binaryclass.task$task.desc$negative))
  df = getTaskData(binaryclass.task, recode.target="-1+1")
  expect_equal(df[,1:20], binaryclass.df[, 1:20])
  expect_true(is.numeric(df[, binaryclass.target]))
  expect_equal(sum(df[, binaryclass.target] == 1), 
    sum(binaryclass.df[, binaryclass.target] == binaryclass.task$task.desc$positive))
  expect_equal(sum(df[, binaryclass.target] == -1), 
    sum(binaryclass.df[, binaryclass.target] == binaryclass.task$task.desc$negative))

  df = getTaskData(binaryclass.task, subset=1:150, features=colnames(binaryclass.df)[1:2])
  expect_equal(nrow(df), 150)
  expect_equal(ncol(df), 3)
  df = getTaskData(binaryclass.task, subset=1:150, features=colnames(binaryclass.df)[1:2], 
    recode.target="01")
  expect_equal(nrow(df), 150)
  expect_equal(ncol(df), 3)
  
  x = getTaskData(multiclass.task, target.extra=TRUE)
  expect_equal(x$data[,1:4], multiclass.df[,1:4])
  expect_equal(x$target, multiclass.df[, multiclass.target])
})