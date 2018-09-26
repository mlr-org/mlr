context("helpers")

test_that("makeOptPathDFFromMeasures", {
  measures = list(mae, auc)
  par.set = makeParamSet(
    makeIntegerParam("a"),
    makeIntegerParam("b")
  )
  opt.path = makeOptPathDFFromMeasures(par.set, measures)
  addOptPathEl(opt.path, x = list(1L, 1L), y = c(0.5, 0.5))
  res = getOptPathEl(opt.path, 1L)
  supposed.names = sapply(measures, function(m) paste(m$id, m$aggr$id, sep = "."))
  expect_equal(names(res$y), supposed.names)
})

test_that("propVectorToMatrix", {
  x = seq(0, 1, length.out = 11)
  levs = LETTERS[1:2]
  m = propVectorToMatrix(x, levs)
  expect_equal(m[, 1], 1 - x)
  expect_equal(m[, 2], x)
  expect_equal(colnames(m), levs)
})

test_that("listTaskTypes", {
  expected = c("classif", "regr", "surv", "costsens", "cluster", "multilabel", "fcregr", "mfcregr")
  expect_equal(expected, listTaskTypes())
})

test_that("listLearnerProperties", {
  expected = c("classif", "regr", "surv", "costsens", "cluster", "multilabel", "fcregr", "mfcregr")
  expect_equal(expected, listTaskTypes())
})


test_that("suppressWarning works", {
  foo = function(x) {
    if (x > 3)
      warning("x is pretty large.")
    x
  }

  expect_equal(suppressWarning(foo(3), "pretty"), 3)
  expect_equal(suppressWarning(foo(3), "<nomatch>"), 3)
  expect_equal(suppressWarning(foo(4), "pretty"), 4)
  expect_warning(suppressWarning(foo(4), "<nomatch>"), "pretty")
})
