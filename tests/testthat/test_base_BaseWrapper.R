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

test_that("Joint model performance estimation, tuning, and model performance", {
  lrn = makeLearner("classif.ksvm", predict.type = "prob")
  lrn2 = makeTuneWrapper(
    learner = lrn,
    par.set = makeParamSet(
      makeDiscreteParam("C", values = 2^(-2:2)),
      makeDiscreteParam("sigma", values = 2^(-2:2))
    ),
    measures = list(auc, acc),
    control = makeTuneControlRandom(maxit = 3L),
    resampling = makeResampleDesc(method = "Holdout")
  )
  lrn3 = makeFeatSelWrapper(
    learner = lrn2,
    measures = list(auc, acc),
    control = makeFeatSelControlRandom(maxit = 3L),
    resampling = makeResampleDesc(method = "Holdout")
  )
  bmrk = benchmark(lrn3, pid.task, makeResampleDesc(method = "Holdout"), measures = getDefaultMeasure(pid.task))
  expect_is(bmrk, "BenchmarkResult")
})

test_that("Error when wrapping tune wrapper around another optimization wrapper", {
  expect_error({
    lrn = makeLearner("classif.ksvm", predict.type = "prob")
    lrn2 = makeFeatSelWrapper(
      learner = lrn,
      measures = list(auc, acc),
      control = makeFeatSelControlRandom(maxit = 3L),
      resampling = makeResampleDesc(method = "Holdout")
    )
    lrn3 = makeTuneWrapper(
      learner = lrn2,
      par.set = makeParamSet(
        makeDiscreteParam("C", values = 2^(-2:2)),
        makeDiscreteParam("sigma", values = 2^(-2:2))
      ),
      measures = list(auc, acc),
      control = makeTuneControlRandom(maxit = 3L),
      resampling = makeResampleDesc(method = "Holdout")
    )
    bmrk = benchmark(lrn3, pid.task, resampling, measures = getDefaultMeasure(pid.task))
  }, "Cannot wrap a tuning wrapper around another optimization wrapper!")
})
