context("tuneParams")

test_that("names for minimize are set correctly", {
  lrn = makeLearner("classif.ksvm")
  rdesc = makeResampleDesc("Holdout")
  ps = makeParamSet(
    makeNumericParam("C", lower = 0.001, upper = 1)
  )
  ctrl = makeTuneControlRandom(maxit = 2)
  tr = tuneParams(lrn, multiclass.task, rdesc, measures = list(foo = acc),
    par.set = ps, control = ctrl)
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
  tr = tuneParams(lrn, multiclass.task, rdesc, par.set = ps, control = ctrl,
    resample.fun = constant05Resample)
  expect_true(all(getOptPathY(tr$opt.path) == 0.5))

  ctrl = makeTuneControlGrid(resolution = 3L)
  tr = tuneParams(lrn, multiclass.task, rdesc, par.set = ps, control = ctrl,
    resample.fun = constant05Resample)
  expect_true(all(getOptPathY(tr$opt.path) == 0.5))

  ctrl = makeTuneControlIrace(maxExperiments = 20L, nbIterations = 1L,
    minNbSurvival = 1)
  tr = tuneParams(lrn, multiclass.task, rdesc, par.set = ps, control = ctrl,
    resample.fun = constant05Resample)
  expect_true(all(getOptPathY(tr$opt.path) == 0.5))

  ctrl = makeTuneControlCMAES(start = list(cp = 0.05, minsplit = 5L), maxit = 5)
  tr = tuneParams(lrn, multiclass.task, rdesc, par.set = ps, control = ctrl,
    resample.fun = constant05Resample)
  expect_true(all(getOptPathY(tr$opt.path) == 0.5))

  ctrl = makeTuneControlGenSA(start = list(cp = 0.05, minsplit = 5L), maxit = 5)
  tr = tuneParams(lrn, multiclass.task, rdesc, par.set = ps, control = ctrl,
    resample.fun = constant05Resample)
  expect_true(all(getOptPathY(tr$opt.path) == 0.5))

  ctrl = suppressWarnings({
    # this currently is a warning because printHead is in mlr and BBmisc
    makeTuneControlMBO(budget = 10, learner = "regr.lm")
  })
  tr = tuneParams(lrn, multiclass.task, rdesc, par.set = ps, control = ctrl,
    resample.fun = constant05Resample)
  expect_true(all(getOptPathY(tr$opt.path) == 0.5))
})

test_that("tuneParams output works as documented", {
  lrn = makeLearner("classif.ksvm")
  rdesc = makeResampleDesc("Holdout")
  ps = makeParamSet(
    makeNumericParam("C", lower = 0.001, upper = 1)
  )
  ctrl.default = makeTuneControlRandom(maxit = 2)
  ctrl.memory = makeTuneControlRandom(maxit = 2, log.fun = "memory")
  ctrl.user = makeTuneControlRandom(maxit = 2, log.fun = function(learner,
    task, resampling, measures, par.set, control, opt.path, dob, x, y,
    remove.nas, stage, prev.stage) {
    message("Hi")
  })

  expect_message(tuneParams(lrn, multiclass.task, rdesc,
    measures = list(foo = acc), par.set = ps, control = ctrl.default,
    show.info = TRUE),
  "\\[Tune-y\\] \\d+: [^;]+; time:[^;]+$")

  expect_message(tuneParams(lrn, multiclass.task, rdesc,
    measures = list(foo = acc), par.set = ps, control = ctrl.memory,
    show.info = TRUE),
  "\\[Tune-y\\] \\d+: [^;]+; time:[^;]+; memory:[^;]+$")

  expect_message(tuneParams(lrn, multiclass.task, rdesc,
    measures = list(foo = acc), par.set = ps, control = ctrl.user,
    show.info = TRUE),
  "^Hi")
})

test_that("tuneParams output works as documented", {
  lrn = makeLearner("classif.ksvm")
  rdesc = makeResampleDesc("Holdout")
  ps = makeParamSet(
    makeNumericParam("C", lower = 0.001, upper = 1)
  )
  ctrl.default = makeTuneControlRandom(maxit = 2)
  ctrl.memory = makeTuneControlRandom(maxit = 2, log.fun = "memory")
  ctrl.user = makeTuneControlRandom(maxit = 2, log.fun = function(learner, task,
    resampling, measures, par.set, control, opt.path, dob, x, y, remove.nas,
    stage, prev.stage) {
    message("Hi")
  })

  expect_message(tuneParams(lrn, multiclass.task, rdesc,
    measures = list(foo = acc), par.set = ps, control = ctrl.default,
    show.info = TRUE),
  "\\[Tune-y\\] \\d+: [^;]+; time:[^;]+$")

  expect_message(tuneParams(lrn, multiclass.task, rdesc,
    measures = list(foo = acc), par.set = ps, control = ctrl.memory,
    show.info = TRUE),
  "\\[Tune-y\\] \\d+: [^;]+; time:[^;]+; memory:[^;]+$")

  expect_message(tuneParams(lrn, multiclass.task, rdesc,
    measures = list(foo = acc), par.set = ps, control = ctrl.user,
    show.info = TRUE),
  "^Hi")
})

test_that("tuning with a fixed ensemble methods and varying base methods works", {

  # TODO: make it possible to choose arbitrary number of base.methods -> cannot
  # tune an argument of a param. We need to make makeDiscreteVectorParam more
  # flexible to allow more than one ensemble.method
  lrn = makeFilterWrapper(learner = "classif.ksvm", fw.method = "E-min")

  filter.list = listFilterMethods(desc = FALSE, tasks = TRUE, features = FALSE)
  filter.list.classif = as.character(filter.list$id)[filter.list$task.classif]
  filter.list.classif = setdiff(filter.list.classif, c(
    "univariate.model.score", "permutation.importance", "auc",
    "univariate", "rf.importance", "randomForestSRC_var.select"))

  ps = makeParamSet(
    makeDiscreteVectorParam("fw.base.methods", len = 2,
      values = filter.list.classif),
    makeNumericParam("fw.perc", lower = 0, upper = 1),
    makeNumericParam("C", lower = -10, upper = 10,
      trafo = function(x) 2^x),
    makeNumericParam("sigma", lower = -10, upper = 10,
      trafo = function(x) 2^x)
  )
  rdesc = makeResampleDesc("CV", iters = 3)
  out = tuneParams(lrn, task = iris.task, resampling = rdesc, par.set = ps,
    control = makeTuneControlRandom(maxit = 5), show.info = TRUE)
})

test_that("tuning with a fixed ensemble methods and varying base methods works", {

  # TODO: choose arbitrary number of base.methods -> cannot tune an argument of
  # a param. We need to make makeDiscreteVectorParam more flexible. allow more
  # than one ensemble.method
  lrn = makeFilterWrapper(learner = "classif.ksvm", fw.method = "E-min")

  filter.list = listFilterMethods(desc = FALSE, tasks = TRUE, features = FALSE)
  filter.list.classif = as.character(filter.list$id)[filter.list$task.classif]
  filter.list.classif = setdiff(filter.list.classif, c(
    "univariate.model.score", "permutation.importance", "auc",
    "univariate", "rf.importance", "randomForestSRC_var.select"))

  ps = makeParamSet(
    makeDiscreteVectorParam("fw.base.methods", len = 2,
      values = filter.list.classif),
    makeNumericParam("fw.perc", lower = 0, upper = 1),
    makeNumericParam("C", lower = -10, upper = 10,
      trafo = function(x) 2^x),
    makeNumericParam("sigma", lower = -10, upper = 10,
      trafo = function(x) 2^x)
  )
  rdesc = makeResampleDesc("CV", iters = 3)
  out = tuneParams(lrn, task = iris.task, resampling = rdesc, par.set = ps,
    control = makeTuneControlRandom(maxit = 5), show.info = TRUE)
})

test_that("tuning with fixed base methods and varying ensemble methods works", {
  lrn = makeFilterWrapper(learner = "classif.ksvm",
    fw.base.methods = c("gain.ratio", "information.gain"))

  ps = makeParamSet(makeDiscreteParam("fw.method", values = c("E-min", "E-max")),
    makeNumericParam("fw.perc", lower = 0, upper = 1),
    makeNumericParam("C", lower = -10, upper = 10,
      trafo = function(x) 2^x),
    makeNumericParam("sigma", lower = -10, upper = 10,
      trafo = function(x) 2^x)
  )
  rdesc = makeResampleDesc("CV", iters = 3)
  out = tuneParams(lrn, task = iris.task, resampling = rdesc, par.set = ps,
    control = makeTuneControlRandom(maxit = 5), show.info = TRUE)
})

test_that("passing more than one fw.method raises an error", {
  expect_error(makeFilterWrapper(learner = "classif.ksvm",
    fw.method = c("E-min", "E-max"),
    fw.base.methods = c("gain.ratio", "information.gain"))
  )
})
