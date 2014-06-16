context("ModelMultiplexer")

test_that("ModelMultiplexer", {
  bls = list(makeLearner("classif.lda"), makeLearner("classif.rpart"))
  lrn = makeModelMultiplexer(bls)
  expect_equal(class(lrn), c("ModelMultiplexer", "Learner"))
  mod = train(lrn, task = binaryclass.task)
  expect_equal(class(mod), "WrappedModel")

  rdesc = makeResampleDesc("CV", iters = 2L)
  res = resample(lrn, binaryclass.task, rdesc)

  lrn = setHyperPars(lrn, selected.learner = "classif.rpart", classif.rpart.minsplit = 100)
  expect_true(setequal(names(getHyperPars(lrn)), c("selected.learner", "classif.rpart.minsplit")))

  tune.ps = makeModelMultiplexerParamSet(lrn,
    classif.rpart = makeParamSet(makeIntegerParam("minsplit", lower = 1, upper = 50))
  )
  ctrl = makeTuneControlRandom(maxit = 4L)
  res = tuneParams(lrn, binaryclass.task, rdesc, par.set = tune.ps, control = ctrl)
  expect_true(setequal(class(res), c("TuneResult", "OptResult")))
})
