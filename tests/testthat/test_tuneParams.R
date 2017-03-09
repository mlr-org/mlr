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

test_that("tuneParams output works as documented", {
  lrn = makeLearner("classif.ksvm")
  rdesc = makeResampleDesc("Holdout")
  ps = makeParamSet(
    makeNumericParam("C", lower = 0.001, upper = 1)
  )
  ctrl_default = makeTuneControlRandom(maxit = 2)
  ctrl_memory = makeTuneControlRandom(maxit = 2, log.fun = "memory")
  ctrl_user = makeTuneControlRandom(maxit = 2, log.fun = function(learner, task, resampling, measures, par.set, control, opt.path, dob, x, y, remove.nas, stage, prev.stage) message("Hi"))

  expect_message(tuneParams(lrn, multiclass.task, rdesc, measures = list(foo = acc), par.set = ps, control = ctrl_default, show.info = TRUE),
    "\\[Tune-y\\] \\d+: [^;]+; time:[^;]+$")

  expect_message(tuneParams(lrn, multiclass.task, rdesc, measures = list(foo = acc), par.set = ps, control = ctrl_memory, show.info = TRUE),
    "\\[Tune-y\\] \\d+: [^;]+; time:[^;]+; memory:[^;]+$")

  expect_message(tuneParams(lrn, multiclass.task, rdesc, measures = list(foo = acc), par.set = ps, control = ctrl_user, show.info = TRUE),
    "^Hi")
})

