context("tuneRandom")

test_that("tuneRandom", {
  lrn = makeLearner("classif.ksvm")
  rdesc = makeResampleDesc("Holdout")
  ps = makeParamSet(
    makeNumericParam("C", lower = 0.001, upper = 1),
    makeDiscreteParam("kernel", values = c("rbfdot", "vanilladot"))
  )

  ctrl = makeTuneControlRandom(maxit = 5)
  tr = tuneParams(lrn, multiclass.task, rdesc, par.set = ps, control = ctrl,
    measures = acc)
  expect_equal(getOptPathLength(tr$opt.path), 5)
  expect_number(tr$y, lower = 0.8, upper = 1)
})

test_that("tuneRandom works with dependent params", {
  lrn = makeLearner("classif.ksvm")
  rdesc = makeResampleDesc("Holdout")
  lrn = makeLearner("classif.ksvm")
  ps = makeParamSet(
    makeDiscreteParam("kernel", values = c("vanilladot", "rbfdot")),
    makeNumericParam("C", lower = 1, upper = 2),
    makeNumericParam("sigma", lower = 1, upper = 2,
      requires = quote(kernel == "rbfdot"))
  )
  ctrl = makeTuneControlRandom(maxit = 5)
  tr = tuneParams(lrn, multiclass.task, rdesc, par.set = ps, control = ctrl)
  expect_equal(getOptPathLength(tr$opt.path), 5)
  expect_number(tr$y, lower = 0, upper = 0.2)
})

test_that("tuneRandom works with trafo", {
  lrn = makeLearner("classif.ksvm")
  ps = makeParamSet(makeNumericParam("sigma", lower = -10, upper = -1,
    trafo = function(x) 2^x))
  rdesc = makeResampleDesc("Holdout")
  ctrl = makeTuneControlRandom(maxit = 3)
  tr = tuneParams(lrn, iris.task, rdesc, par.set = ps, control = ctrl)
  expect_equal(getOptPathLength(tr$opt.path), 3)
  expect_true(!is.na(tr$y) && is.finite(tr$y))
})

test_that("tuneRandom uses budget", {
  lrn = makeLearner("classif.ksvm")
  ps = makeParamSet(makeNumericParam("sigma", lower = -10, upper = -1,
    trafo = function(x) 2^x))
  rdesc = makeResampleDesc("Holdout")
  ctrl = makeTuneControlRandom(maxit = 3)
  ctrl2 = makeTuneControlRandom(budget = 3)
  ctrl3 = makeTuneControlRandom(budget = 3, maxit = 3)
  expect_identical(ctrl, ctrl2)
  expect_identical(ctrl, ctrl3)
  expect_error(makeTuneControlRandom(budget = 3, maxit = 4))

  tr = tuneParams(lrn, iris.task, rdesc, par.set = ps, control = ctrl)
  expect_equal(getOptPathLength(tr$opt.path), 3)
  expect_true(!is.na(tr$y) && is.finite(tr$y))
})
