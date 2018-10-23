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

  ctrl = suppressWarnings({
    # this currently is a warning because printHead is in mlr and BBmisc
     makeTuneControlMBO(budget = 10, learner = "regr.lm")
  })
  tr = tuneParams(lrn, multiclass.task, rdesc, par.set = ps, control = ctrl, resample.fun = constant05Resample)
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
  ctrl.user = makeTuneControlRandom(maxit = 2, log.fun = function(learner, task, resampling, measures, par.set, control, opt.path, dob, x, y, remove.nas, stage, prev.stage) message("Hi"))

  expect_message(tuneParams(lrn, multiclass.task, rdesc, measures = list(foo = acc), par.set = ps, control = ctrl.default, show.info = TRUE),
    "\\[Tune-y\\] \\d+: [^;]+; time:[^;]+$")

  expect_message(tuneParams(lrn, multiclass.task, rdesc, measures = list(foo = acc), par.set = ps, control = ctrl.memory, show.info = TRUE),
    "\\[Tune-y\\] \\d+: [^;]+; time:[^;]+; memory:[^;]+$")

  expect_message(tuneParams(lrn, multiclass.task, rdesc, measures = list(foo = acc), par.set = ps, control = ctrl.user, show.info = TRUE),
    "^Hi")
})


test_that("tuneParams results are equal when using cached filters", {

  filters = as.character(listFilterMethods()$id)
  filter.list = listFilterMethods(desc = FALSE, tasks = TRUE, features = FALSE)
  filter.list.classif = as.character(filter.list$id)[filter.list$task.classif]
  filter.list.classif = setdiff(filter.list.classif, c(
    "univariate.model.score", "permutation.importance", "auc",
    "univariate", "rf.importance", "rf.min.depth"))
  filter.list.regr = as.character(filter.list$id)[!filter.list$task.classif & filter.list$task.regr]

  purrr::walk(filter.list.regr, ~ {
    lrn = makeFilterWrapper(learner = "regr.ksvm", fw.method = .x)
    ps = makeParamSet(makeNumericParam("fw.perc", lower = 0, upper = 1),
                      makeNumericParam("C", lower = -10, upper = 10,
                                       trafo = function(x) 2^x),
                      makeNumericParam("sigma", lower = -10, upper = 10,
                                       trafo = function(x) 2^x)
    )
    rdesc = makeResampleDesc("CV", iters = 3)

    print(.x)

    out = lapply(c(FALSE, TRUE), function (x) {
      tuneParams(lrn, task = regr.num.task, resampling = rdesc, par.set = ps,
                 control = makeTuneControlRandom(maxit = 5),
                 cache = x, show.info = FALSE)
    })
    expect_equal(out[[1]][["opt.path"]][["env"]][["path"]][["mse.test.mean"]],
                 out[[2]][["opt.path"]][["env"]][["path"]][["mse.test.mean"]])
  })

  purrr::walk(filter.list.classif, ~ {
    lrn = makeFilterWrapper(learner = "classif.ksvm", fw.method = .x)
    ps = makeParamSet(makeNumericParam("fw.perc", lower = 0, upper = 1),
                      makeNumericParam("C", lower = -10, upper = 10,
                                       trafo = function(x) 2^x),
                      makeNumericParam("sigma", lower = -10, upper = 10,
                                       trafo = function(x) 2^x)
    )
    rdesc = makeResampleDesc("CV", iters = 3)

    print(.x)

    out = lapply(c(FALSE, TRUE), function (x) {
      tuneParams(lrn, task = multiclass.task, resampling = rdesc, par.set = ps,
                 control = makeTuneControlRandom(maxit = 5),
                 cache = x, show.info = FALSE)
    })
    expect_equal(out[[1]][["opt.path"]][["env"]][["path"]][["mse.test.mean"]],
                 out[[2]][["opt.path"]][["env"]][["path"]][["mse.test.mean"]])
  })
})
