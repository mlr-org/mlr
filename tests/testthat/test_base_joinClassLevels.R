context("joinClassLevels")

test_that("joinClassLevels", {
  expect_error(joinClassLevels(multiclass.task, "foo"), "list")
  expect_error(joinClassLevels(multiclass.task, new.levels = list(c("setosa", "virginica"))))
  expect_error(joinClassLevels(multiclass.task, new.levels = list(a = c("setosa", "virginica1"))), "existing")
  expect_error(joinClassLevels(multiclass.task, new.levels = list(a = c("setosa", "virginica"), b = "virginica")), "once")

  d = getTaskData(joinClassLevels(multiclass.task, new.levels = list(a = c("setosa", "virginica"))))
  expect_true(is.factor(d$Species))
  expect_equal(sum(d$Species == "versicolor"), 50L)
  expect_equal(sum(d$Species == "a"), 100L)
  d = getTaskData(joinClassLevels(multiclass.task, new.levels = list(a = "setosa", b = "virginica")))
  expect_true(is.factor(d$Species))
  expect_equal(sum(d$Species == "versicolor"), 50L)
  expect_equal(sum(d$Species == "a"), 50L)
  expect_equal(sum(d$Species == "b"), 50L)

  task2 = joinClassLevels(multiclass.task, new.levels = list(a = c("setosa", "virginica")))
  d = getTaskData(task2)
  expect_true(is.factor(d$Species))
  expect_equal(sum(d$Species == "versicolor"), 50L)
  expect_equal(sum(d$Species == "a"), 100L)
})
