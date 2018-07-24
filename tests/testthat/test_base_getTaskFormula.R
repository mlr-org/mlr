context("getTaskFormula")

test_that("getTaskFormula", {
  ## binaryclass
  expect_equal(binaryclass.formula, getTaskFormula(binaryclass.task))
  my.binaryclass.formula = paste(binaryclass.target, "~",
    collapse(colnames(binaryclass.df[, -binaryclass.class.col]), sep = " + "))
  expect_equal(as.formula(my.binaryclass.formula),
    getTaskFormula(binaryclass.task, explicit.features = TRUE))

  ## multiclass
  expect_equal(multiclass.formula, getTaskFormula(multiclass.task))
  my.multiclass.formula = paste(multiclass.target, "~",
    collapse(colnames(multiclass.df[, -multiclass.class.col]), sep = " + "))
  expect_equal(as.formula(my.multiclass.formula),
    getTaskFormula(multiclass.task, explicit.features = TRUE))

  ## regr
  expect_equal(regr.formula, getTaskFormula(regr.task))
  my.regr.formula = paste(regr.target, "~",
    collapse(colnames(regr.df[, -regr.class.col]), sep = " + "))
  expect_equal(as.formula(my.regr.formula),
    getTaskFormula(regr.task, explicit.features = TRUE))

  ## regr
  expect_equal(regr.num.formula, getTaskFormula(regr.num.task))
  my.regr.num.formula = paste(regr.num.target, "~",
    collapse(colnames(regr.num.df[, -regr.num.class.col]), sep = " + "))
  expect_equal(as.formula(my.regr.num.formula),
    getTaskFormula(regr.num.task, explicit.features = TRUE))

  ## multilabel
  expect_equal(multilabel.formula, getTaskFormula(multilabel.task))
  my.multilabel.formula = paste(collapse(multilabel.target, " + "), "~",
    collapse(colnames(multilabel.df[, -multilabel.class.col]), sep = " + "))
  expect_equal(as.formula(my.multilabel.formula),
    getTaskFormula(multilabel.task, explicit.features = TRUE))

  ## multiregr
  expect_equal(multiregr.formula, getTaskFormula(multiregr.task))
  my.multiregr.formula = paste(collapse(multiregr.target, " + "), "~",
    collapse(colnames(multiregr.df[, -multiregr.class.col]), sep = " + "))
  expect_equal(as.formula(my.multiregr.formula),
    getTaskFormula(multiregr.task, explicit.features = TRUE))

  ## mixedoutput
  expect_equal(mixedoutput.formula, getTaskFormula(mixedoutput.task))
  my.mixedoutput.formula = paste(collapse(mixedoutput.target, " + "), "~",
    collapse(colnames(mixedoutput.df[, -mixedoutput.class.col]), sep = " + "))
  expect_equal(as.formula(my.mixedoutput.formula),
    getTaskFormula(mixedoutput.task, explicit.features = TRUE))
})

test_that("issue #1467", {
  expect_error(getTaskFormula(unclass(iris.task)), "no applicable method")
})
