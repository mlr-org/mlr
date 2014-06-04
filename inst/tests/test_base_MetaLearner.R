context("MetaLearner")

test_that("MetaLearner", {
  bls = list(makeLearner("classif.lda"), makeLearner("classif.rpart"))
  lrn = makeMetaLearner(bls)
  expect_equal(class(lrn), c("MetaLearner", "Learner"))
  mod = train(lrn, task = binaryclass.task)
  expect_equal(class(mod), "WrappedModel")

  rdesc = makeResampleDesc("CV", iters = 2L)
  res = resample(lrn, binaryclass.task, rdesc)

  lrn = setHyperPars(lrn, selected.learner="classif.rpart", classif.rpart.minsplit=100)
  expect_true(setequal(names(getHyperPars(lrn)), c("selected.learner", "classif.rpart.minsplit")))

  tune.ps = makeParamSet(
    makeDiscreteParam("selected.learner", values=c("classif.lda", "classif.rpart")),
    makeIntegerParam("classif.rpart.minsplit", lower=1, upper=50, requires = quote(selected.learner == "classif.rpart"))
  )
  ctrl = makeTuneControlRandom(maxit = 10)
  res = tuneParams(lrn, binaryclass.task, rdesc, par.set=tune.ps, control=ctrl)
  expect_true(setequal(class(res), c("TuneResult", "OptResult")))
})
