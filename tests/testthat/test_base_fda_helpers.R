context("fda_helpers_task_operators")

test_that("hasFunctionals works", {
  expect_false(hasFunctionalFeatures(iris.task))
  expect_false(hasFunctionalFeatures(iris.task$task.desc))
  expect_false(hasFunctionalFeatures(getTaskData(iris.task)))

  expect_true(hasFunctionalFeatures(fda.binary.gp.task))
  expect_true(hasFunctionalFeatures(fda.binary.gp.task$task.desc))
  expect_true(hasFunctionalFeatures(getTaskData(fda.binary.gp.task, functionals.as = "matrix")))
})

test_that("getTaskData for functional tasks", {

  expect_true(hasFunctionalFeatures(getTaskData(fda.binary.gp.task, functionals.as = "matrix")))
  expect_message({df = getTaskData(fda.binary.gp.task, subset = 1:50, functionals.as = "dfCols")})
  expect_false(hasFunctionalFeatures(df))

  # Subset rows
  expect_true(hasFunctionalFeatures(getTaskData(fda.binary.gp.task, subset = 1:50, functionals.as = "matrix")))
  expect_message({df = getTaskData(fda.binary.gp.task, subset = 1:50, functionals.as = "dfCols")})
  expect_false(hasFunctionalFeatures(df))

  # We can not really subset cols for this task.
  expect_false(hasFunctionalFeatures(getTaskData(fda.regr.fs.task, features = 1, functionals.as = "matrix")))
  expect_true(hasFunctionalFeatures(getTaskData(fda.regr.fs.task, features = 2, functionals.as = "matrix")))
  expect_true(hasFunctionalFeatures(getTaskData(fda.regr.fs.task, features = 3, functionals.as = "matrix")))
  expect_silent({df = getTaskData(fda.regr.fs.task, features = 1, functionals.as = "dfCols")})
  expect_false(hasFunctionalFeatures(df))
  expect_message({df = getTaskData(fda.regr.fs.task, features = c(2, 3), functionals.as = "dfCols")})
  expect_false(hasFunctionalFeatures(df))


  expect_error(hasFunctionalFeatures(getTaskData(fda.binary.gp.task, features = 2, functionals.as = "matrix")))
  expect_error(hasFunctionalFeatures(getTaskData(fda.binary.gp.task, features = 2, functionals.as = "dfCols")))

  expect_false(hasFunctionalFeatures(getTaskData(iris.task, functionals.as = "matrix")))
  expect_silent({df = getTaskData(iris.task, subset = 1:50, functionals.as = "matrix")})
  expect_false(hasFunctionalFeatures(df))
  expect_false(hasFunctionalFeatures(getTaskData(iris.task, functionals.as = "dfCols")))
  expect_silent({df = getTaskData(iris.task, subset = 1:50, functionals.as = "dfCols")})
  expect_false(hasFunctionalFeatures(df))

})
