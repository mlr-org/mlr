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
  expect_equal(m[,1], 1 - x)
  expect_equal(m[,2], x)
  expect_equal(colnames(m), levs)
})

test_that("listTaskTypes", {
  expected = c("classif", "regr", "surv", "costsens", "cluster", "multilabel")
  expect_equal(expected, listTaskTypes())
})

test_that("listLearnerProperties", {
  expected = c("classif", "regr", "surv", "costsens", "cluster", "multilabel")
  expect_equal(expected, listTaskTypes())
})

test_that("simplifyMeasureNames", {
  # setup some measures get ids and aggegated names
  meas = list(mmce, acc, ber)
  meas.aggr = vcapply(meas, measureAggrName)
  meas.ids = extractSubList(meas, "id")
  # some dummy-strings not representing measures
  no.meas = c("abc", "def")
  # join aggr.names and dummy entries together
  xs = c(meas.aggr, no.meas)
  # test that aggr names get clipped and dummies are unchanged
  expected = c(meas.ids, no.meas)
  expect_equal(expected, simplifyMeasureNames(xs))

  # check measure ids are ignored too
  xs = c("acc", "no measure")
  expect_equal(xs, simplifyMeasureNames(xs))
})
