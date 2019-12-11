context("BaseEnsemble")

test_that("BaseEnsemble", {
  bl1 = makeLearner("classif.rpart", minsplit = 2L, id = "a")
  bl2 = makeLearner("classif.ksvm", C = 2, id = "b")
  ps = makeParamSet(makeNumericLearnerParam("foo"))
  pv = list(foo = 3)
  be = makeBaseEnsemble(id = "foo", base.learners = list(bl1, bl2), par.set = ps, par.vals = pv,
    cl = "mywrapper")
  expect_true(setequal(getHyperPars(be), list(a.xval = 0L, a.minsplit = 2L,
    b.fit = FALSE, b.C = 2, foo = 3)))

  be = setHyperPars(be, a.minsplit = 11)
  expect_true(setequal(getHyperPars(be), list(a.xval = 0L, a.minsplit = 11L,
    b.fit = FALSE, b.C = 2, foo = 3)))

  be = setHyperPars(be, foo = 12)
  expect_true(setequal(getHyperPars(be), list(a.xval = 0L, a.minsplit = 11L,
    b.fit = FALSE, b.C = 2, foo = 12)))

  # check removing hyperpars
  be1 = removeHyperPars(be, names(getHyperPars(be)))
  expect_true(length(getHyperPars(be1)) == 0)

  bl1 = be
  bl2 = makeOversampleWrapper(makeFilterWrapper(bl2, fw.perc = 0.5), osw.rate = 1)
  ps = makeParamSet(makeNumericLearnerParam("foo"))
  pv = list(foo = 3)
  be = makeBaseEnsemble(id = "foo", base.learners = list(bl1, bl2), par.set = ps, par.vals = pv,
    cl = "mywrapper")
  be1 = removeHyperPars(be, names(getHyperPars(be)))

  # check that we get error if predict types are unequal
  bl1 = makeLearner("classif.rpart", predict.type = "prob")
  bl2 = makeLearner("classif.ksvm", predict.type = "response")
  expect_error(makeBaseEnsemble(id = "foo", base.learners = list(bl1, bl2),
    par.set = ps, par.vals = pv, cl = "mywrapper"), "predict.type")

  # check getHyperPars when we have multiple wrappers
  bl1 = makeLearner("classif.rpart", minsplit = 2L, id = "rpart")
  bl2 = makeLearner("classif.ksvm", C = 2, id = "ksvm")
  # now make a wrapper around bl2 (ksvm)
  bl2 = makeOversampleWrapper(makeFilterWrapper(bl2, fw.perc = 0.5), osw.rate = 1)
  be = makeBaseEnsemble(id = "foo", base.learners = list(bl1, bl2), cl = "mywrapper")
  expect_output(print(be), "mywrapper")
  expect_true(setequal(getHyperPars(be),
    list(rpart.xval = 0L, rpart.minsplit = 2L,
      ksvm.filtered.oversampled.fit = FALSE,
      ksvm.filtered.oversampled.C = 2,
      ksvm.filtered.oversampled.fw.method = "randomForestSRC_importance",
      ksvm.filtered.oversampled.fw.perc = 0.5,
      ksvm.filtered.oversampled.osw.rate = 1)))

  # check removing hyperpars
  be.rm = removeHyperPars(be, names(getHyperPars(be)))
  expect_true(length(getHyperPars(be.rm)) == 0)

  # check setPredictType
  be.pt = setPredictType(be, predict.type = "prob")
  expect_equal(be.pt$predict.type, "prob")
  expect_equal(lapply(be.pt$base.learners, function(x) x$predict.type), list(rpart = "prob", ksvm.filtered.oversampled = "prob"))

  be.pt = setPredictType(be, predict.type = "response")
  expect_equal(be.pt$predict.type, "response")
  expect_equal(lapply(be.pt$base.learners, function(x) x$predict.type), list(rpart = "response", ksvm.filtered.oversampled = "response"))
})
