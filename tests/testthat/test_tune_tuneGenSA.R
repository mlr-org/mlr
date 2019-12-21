context("tuneGenSA")

test_that("tuneGenSA", {
  res = makeResampleDesc("CV", iters = 2)
  ps1 = makeParamSet(
    makeNumericParam("cp", lower = 0.001, upper = 1),
    makeIntegerParam("minsplit", lower = 1, upper = 10)
  )
  ctrl1 = makeTuneControlGenSA(start = list(cp = 0.05, minsplit = 5L),
    maxit = 5)
  tr1 = tuneParams(makeLearner("classif.rpart"), multiclass.task, res,
    par.set = ps1, control = ctrl1, measures = acc)
  expect_number(tr1$y, lower = 0.8, upper = 1)

  ps2 = makeParamSet(
    makeNumericVectorParam("cutoff", lower = 0.0001, upper = 1, len = 3,
      trafo = function(x) x / (1.1 * sum(x))),
    makeIntegerParam("ntree", lower = 100, upper = 500)
  )

  ctrl2 = makeTuneControlGenSA(start = list(cutoff = c(1 / 3, 1 / 3, 1 / 3),
    ntree = 200L),
  maxit = 5, max.call = 3)
  tr2 = tuneParams(makeLearner("classif.randomForest"), multiclass.task, res,
    par.set = ps2, control = ctrl2)
  expect_equal(ncol(as.data.frame(tr2$opt.path)), 4 + 1 + 2 + 2)
  expect_number(tr2$y, lower = 0, upper = 0.2)
  expect_true(is.list(tr2$x))
  expect_equal(length(tr2$x), 2)

  ps3 = makeParamSet(
    makeNumericParam("cp", lower = 0.001, upper = 1),
    makeDiscreteParam("minsplit", values = c(1, 2))
  )
  expect_error(tuneParams(makeLearner("classif.rpart"), multiclass.task, res,
    par.set = ps3, control = ctrl1))
})

test_that("tuneGenSA with budget", {
  res = makeResampleDesc("CV", iters = 2)
  ps1 = makeParamSet(
    makeNumericParam("cp", lower = 0.001, upper = 1),
    makeIntegerParam("minsplit", lower = 1, upper = 10)
  )
  expect_error((makeTuneControlGenSA(start = list(cp = 0.05, minsplit = 5L),
    max.call = 5, budget = 8)))
  ctrl1 = makeTuneControlGenSA(start = list(cp = 0.05, minsplit = 5L),
    max.call = 30, budget = 30)
  tr1 = tuneParams(makeLearner("classif.rpart"), multiclass.task, res,
    par.set = ps1, control = ctrl1)
  expect_identical(getOptPathLength(tr1$opt.path), ctrl1$budget)
  expect_identical(getOptPathLength(tr1$opt.path), ctrl1$extra.args$max.call)
})
