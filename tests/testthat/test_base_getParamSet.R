context("getParamSet")

test_that("getParamSet", {
  lrn = makeLearner("classif.lda")
  ps = getParamSet(lrn)
  expect_true(setequal(names(ps$pars), c("method", "nu", "tol", "predict.method", "CV", "prior")))

  lrn = makeFilterWrapper(lrn)
  ps = getParamSet(lrn)
  expect_true(all(c("method", "fw.method") %in% names(ps$pars)))

  lrn = makeModelMultiplexer(list(setLearnerId(lrn, "x")))
  ps = getParamSet(lrn)
  expect_true(all(c("x.method", "x.fw.method", "selected.learner") %in% names(ps$pars)))

  expect_true("type" %in% names(getParamSet("classif.ksvm")$pars))
})
