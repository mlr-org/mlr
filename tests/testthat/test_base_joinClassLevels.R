context("joinClassLevels")

test_that("joinClassLevels", {
  expect_error(joinClassLevels(iris, "foo"))
  expect_error(joinClassLevels(iris, "Species", new.levels = list(c("setosa", "virginica"))))
  expect_error(joinClassLevels(iris, "Species", new.levels = list(a = c("setosa", "virginica1"))))
  expect_error(joinClassLevels(iris, "Species", new.levels =
      list(a = c("setosa", "virginica"), b = "virginica")), "once")

  d = joinClassLevels(iris, "Species", new.levels = list(a = c("setosa", "virginica")))
  expect_true(is.factor(d$Species))
  expect_equal(sum(d$Species == "versicolor"), 50L)
  expect_equal(sum(d$Species == "a"), 100L)
  d = joinClassLevels(iris, "Species", new.levels = list(a = "setosa", b = "virginica"))
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



