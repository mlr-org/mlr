context("tuneParams")

test_that("names for minimize are set correctly", {
  lrn = makeLearner("classif.ksvm")
  rdesc = makeResampleDesc("Holdout")
  ps = makeParamSet(
    makeNumericParam("C", lower = 0.001, upper = 1)
  )
  ctrl = makeTuneControlRandom(maxit = 2)
  tr = tuneParams(lrn, multiclass.task, rdesc, measures = list(foo = acc), par.set = ps, control = ctrl)
  expect_is(tr, "TuneResult")
  expect_equal(names(tr$opt.path$minimize), "acc.test.mean")
})

test_that("tuneParams with resample.fun", {
  lrn = makeLearner("classif.rpart")
  rdesc = makeResampleDesc("Holdout")
  ps = makeParamSet(
    makeNumericParam("cp", lower = 0.001, upper = 1),
    makeIntegerParam("minsplit", lower = 1, upper = 10)
  )
  ctrl = makeTuneControlRandom(maxit = 2)
  tr = tuneParams(lrn, multiclass.task, rdesc, par.set = ps, control = ctrl, resample.fun = constant05Resample)
  expect_true(all(getOptPathY(tr$opt.path) == 0.5))

  ctrl = makeTuneControlGrid(resolution = 3L)
  tr = tuneParams(lrn, multiclass.task, rdesc, par.set = ps, control = ctrl, resample.fun = constant05Resample)
  expect_true(all(getOptPathY(tr$opt.path) == 0.5))

  ctrl = makeTuneControlIrace(maxExperiments = 20L, nbIterations = 1L, minNbSurvival = 1)
  tr = tuneParams(lrn, multiclass.task, rdesc, par.set = ps, control = ctrl, resample.fun = constant05Resample)
  expect_true(all(getOptPathY(tr$opt.path) == 0.5))

  ctrl = makeTuneControlCMAES(start = list(cp = 0.05, minsplit = 5L), maxit = 5)
  tr = tuneParams(lrn, multiclass.task, rdesc, par.set = ps, control = ctrl, resample.fun = constant05Resample)
  expect_true(all(getOptPathY(tr$opt.path) == 0.5))

  ctrl = makeTuneControlGenSA(start = list(cp = 0.05, minsplit = 5L), maxit = 5)
  tr = tuneParams(lrn, multiclass.task, rdesc, par.set = ps, control = ctrl, resample.fun = constant05Resample)
  expect_true(all(getOptPathY(tr$opt.path) == 0.5))
})

test_that("tuneParamsMBO does not accept resample.fun", {
  # tuneMBO does currently not implement resample.fun; FIXME change here as soon as it does.
  skip_on_cran() # FIXME remove if mbo is on cran
  skip_if_not_installed("mlrMBO")
  attachNamespace("mlrMBO")
  lrn = makeLearner("classif.rpart")
  rdesc = makeResampleDesc("Holdout")
  ps = makeParamSet(
    makeNumericParam("cp", lower = 0.001, upper = 1),
    makeIntegerParam("minsplit", lower = 1, upper = 10)
  )
 
  mbo.ctrl = makeMBOControl(save.on.disk.at = integer(0L))
  mbo.ctrl = setMBOControlTermination(mbo.ctrl, iters = 2)
  ctrl = makeTuneControlMBO(learner = makeLearner("regr.lm"), mbo.control = mbo.ctrl)
  expect_error(tuneParams(lrn, multiclass.task, rdesc, par.set = ps, control = ctrl, resample.fun = constant05Resample))
})
