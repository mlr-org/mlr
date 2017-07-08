context("BaseWrapper")

test_that("BaseWrapper", {
  lrn1 = makeLearner("classif.rpart", minsplit = 2L)
  ps = makeParamSet(makeNumericLearnerParam("foo"))
  pv = list(foo = 3)
  lrn2 = makeBaseWrapper(id = "foo", lrn1$type, lrn1, par.set = ps, par.vals = pv,
    learner.subclass = "mywrapper", model.subclass = "mymodel")
  expect_equal(getHyperPars(lrn2), list(xval = 0L, minsplit = 2L, foo = 3))
  lrn2 = setHyperPars(lrn2, minsplit = 11)
  expect_equal(getHyperPars(lrn2), list(xval = 0L, minsplit = 11L, foo = 3))
  lrn2 = setHyperPars(lrn2, foo = 12)
  expect_equal(getHyperPars(lrn2), list(xval = 0L, minsplit = 11L, foo = 12))
  lrn2 = setHyperPars(lrn2, foo = 12)
  expect_equal(getHyperPars(lrn2), list(xval = 0L, minsplit = 11L, foo = 12))
  lrn2.rm = removeHyperPars(lrn2, names(getHyperPars(lrn2)))
  expect_equal(length(getHyperPars(lrn2.rm)), 0)

  lrn1 = makeOversampleWrapper(makeFilterWrapper(lrn1, fw.perc = 0.5), osw.rate = 1)
  lrn2 = makeBaseWrapper(id = "foo", lrn1$type, lrn1, par.set = ps, par.vals = pv,
    learner.subclass = "mywrapper", model.subclass = "mymodel")
  lrn2.rm = removeHyperPars(lrn2, names(getHyperPars(lrn2)))
  expect_equal(length(getHyperPars(lrn2.rm)), 0)
})
