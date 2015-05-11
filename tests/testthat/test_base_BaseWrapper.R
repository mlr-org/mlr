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
})
