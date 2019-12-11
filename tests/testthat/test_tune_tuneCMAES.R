context("tuneCMAES")

test_that("tuneCMAES", {
  res = makeResampleDesc("CV", iters = 2)
  ps1 = makeParamSet(
    makeNumericParam("cp", lower = 0.001, upper = 1),
    makeIntegerParam("minsplit", lower = 1, upper = 10)
  )
  ctrl1 = makeTuneControlCMAES(start = list(cp = 0.05, minsplit = 5L),
    maxit = 5)
  tr1 = tuneParams(makeLearner("classif.rpart"), multiclass.task, res,
    par.set = ps1, control = ctrl1)
  expect_number(tr1$y, lower = 0, upper = 0.2)

  ps2 = makeParamSet(
    makeNumericVectorParam("cutoff", lower = 0.0001, upper = 1, len = 3,
      trafo = function(x) x / (1.1 * sum(x))),
    makeIntegerParam("ntree", lower = 100, upper = 500)
  )

  ctrl2 = makeTuneControlCMAES(start = list(cutoff = c(1 / 3, 1 / 3, 1 / 3),
    ntree = 200L),
  maxit = 5, sigma = 2)
  tr2 = tuneParams(makeLearner("classif.randomForest"), multiclass.task, res,
    par.set = ps2, control = ctrl2, measures = acc)
  expect_equal(ncol(as.data.frame(tr2$opt.path)), 4 + 1 + 2 + 2)
  expect_number(tr2$y, lower = 0.8, upper = 1)
  expect_equal(length(tr2$y), 1)
  expect_true(is.list(tr2$x))
  expect_equal(length(tr2$x), 2)

  ps3 = makeParamSet(
    makeNumericParam("cp", lower = 0.001, upper = 1),
    makeDiscreteParam("minsplit", values = c(1, 2))
  )
  expect_error(tuneParams(makeLearner("classif.rpart"), multiclass.task, res,
    par.set = ps3, control = ctrl1))
})

test_that("tuneCMAES with budget", {
  res = makeResampleDesc("CV", iters = 2)
  ps1 = makeParamSet(
    makeNumericParam("cp", lower = 0.001, upper = 1),
    makeIntegerParam("minsplit", lower = 1, upper = 10)
  )
  ctrl = makeTuneControlCMAES(start = list(cp = 0.05, minsplit = 5L), maxit = 5,
    budget = 20)
  expect_error(tuneParams(makeLearner("classif.rpart"), multiclass.task, res,
    par.set = ps1, control = ctrl))
  ctrl1 = makeTuneControlCMAES(start = list(cp = 0.05, minsplit = 5L),
    maxit = 4, budget = 24)
  expect_null(ctrl1$extra.args$lambda)
  expect_equal(ctrl1$extra.args$maxit, 4L)
  expect_identical(length(ctrl1$extra.args$lambda * ctrl1$extra.args$maxit), 0L)
  expect_identical(length((ctrl1$extra.args$lambda + 1) *
    ctrl1$extra.args$maxit), 0L)
  expect_identical(length(ctrl1$extra.args$lambda *
    (ctrl1$extra.args$maxit + 1)), 0L)
  tr1 = tuneParams(makeLearner("classif.rpart"), multiclass.task, res,
    par.set = ps1, control = ctrl1)
  expect_identical(getOptPathLength(tr1$opt.path), ctrl1$budget)
})
