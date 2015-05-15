context("tuneCMAES")

test_that("tuneCMAES", {
  res = makeResampleDesc("CV", iters = 2)
  ps1 = makeParamSet(
    makeNumericParam("cp", lower = 0.001, upper = 1),
    makeIntegerParam("minsplit", lower = 1, upper = 10)
  )
  ctrl1 = makeTuneControlCMAES(start = list(cp = 0.05, minsplit = 5L), budget = 20L)
  expect_identical(ctrl1$budget, 20L)
  expect_true(ctrl1$extra.args$lambda * ctrl1$extra.args$maxit <= 20L)
  expect_true(ctrl1$extra.args$lambda * (ctrl1$extra.args$maxit + 1) > 20L)
  expect_true((ctrl1$extra.args$lambda + 1) * ctrl1$extra.args$maxit > 20L)
  tr1 = tuneParams(makeLearner("classif.rpart"), multiclass.task, res,
    par.set = ps1, control = ctrl1)

  ps2 = makeParamSet(
    makeNumericVectorParam("cutoff", lower = 0.0001, upper = 1, len = 3,
      trafo = function(x) x / (1.1*sum(x))),
    makeIntegerParam("ntree", lower = 100, upper = 500)
  )

  ctrl2 = makeTuneControlCMAES(start = list(cutoff = c(1/3, 1/3, 1/3), ntree = 200L),
    budget = 30, sigma = 2)
  tr2 = tuneParams(makeLearner("classif.randomForest"), multiclass.task, res,
    par.set = ps2, control = ctrl2)
  expect_equal(ncol(as.data.frame(tr2$opt.path)), 4+1+2+2)
  expect_true(is.numeric(tr2$y))
  expect_equal(length(tr2$y), 1)
  expect_true(is.list(tr2$x))
  expect_equal(length(tr2$x), 2)

  ps3 = makeParamSet(
    makeNumericParam("cp", lower = 0.001, upper = 1),
    makeDiscreteParam("minsplit", values = c(1,2))
  )
  expect_error(tuneParams(makeLearner("classif.rpart"), multiclass.task, res,
    par.set = ps3, control = ctrl1))
})

test_that("makeTuneControlCMAES produces an error, if budget setting is not appropriate", {
  expect_error(makeTuneControlCMAES(start = list(cp = 0.05, minsplit = 5L), maxit = 5L))
  expect_error(makeTuneControlCMAES(start = list(cp = 0.05, minsplit = 5L), budget = 10L, maxit = 15L))
})
