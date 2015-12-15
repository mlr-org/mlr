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

  # check that we get error if predict types are unequal
  bl1 = makeLearner("classif.rpart", predict.type = "prob")
  bl2 = makeLearner("classif.ksvm", predict.type = "response")
  expect_error(makeBaseEnsemble(id = "foo", base.learners = list(bl1, bl2),
    par.set = ps, par.vals = pv, cl = "mywrapper"), "predict.type")
})

