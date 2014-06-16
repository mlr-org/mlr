context("ModelMultiplexer")


test_that("makeModelMultiplexerParamSet works", {
  bls = list(
    makeLearner("classif.ksvm"),
    makeLearner("classif.randomForest")
  )

  lrn = makeModelMultiplexer(bls)

  ps1 = makeModelMultiplexerParamSet(lrn,
    makeNumericParam("sigma", lower = -10, upper = 10, trafo = function(x) 2^x),
    makeIntegerParam("ntree", lower = 1L, upper = 500L)
  )

  ps2 = makeModelMultiplexerParamSet(lrn,
    classif.ksvm = makeParamSet(makeNumericParam("sigma", lower = -10, upper = 10, trafo = function(x) 2^x)),
    classif.randomForest = makeParamSet(makeIntegerParam("ntree", lower = 1L, upper = 500L))
  )

  ps3 = makeParamSet(
    makeDiscreteParam("selected.learner", values = extractSubList(bls, "id")),
    makeNumericParam("classif.ksvm.sigma", lower=-10, upper = 10, trafo = function(x) 2^x,
      requires = quote(selected.learner == "classif.ksvm")),
    makeIntegerParam("classif.randomForest.ntree", lower = 1L, upper = 500L,
      requires = quote(selected.learner == "classif.randomForest"))
   )

  expect_equal(ps1, ps2)
  expect_equal(ps2, ps3)
  expect_equal(ps1, ps3)
})


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
    makeIntegerParam("minsplit", lower = 1, upper = 50))
  # tune with random
  ctrl = makeTuneControlRandom(maxit = 4L)
  res = tuneParams(lrn, binaryclass.task, rdesc, par.set = tune.ps, control = ctrl)
  expect_true(setequal(class(res), c("TuneResult", "OptResult")))
  y = getOptPathY(res$opt.path)
  expect_true(!is.na(y) && is.finite(y))
  # tune with irace
  ctrl = makeTuneControlIrace(maxExperiments = 80L)
  res = tuneParams(lrn, binaryclass.task, rdesc, par.set = tune.ps, control = ctrl)
  expect_true(setequal(class(res), c("TuneResult", "OptResult")))
  y = getOptPathY(res$opt.path)
  expect_true(!is.na(y) && is.finite(y))
})
