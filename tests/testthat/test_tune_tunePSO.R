# -----------------------------------------------------------------------------------------------

context("tunePSO")

# -----------------------------------------------------------------------------------------------

test_that("tunePSO", {
  res = makeResampleDesc("CV", iters = 2L)
  ps1 = makeParamSet(
    makeNumericParam("cp", lower = 0.001, upper = 1),
    makeIntegerParam("minsplit", lower = 1, upper = 10)
  )
  ctrl1 = makeTuneControlPSO(start = list(cp = 0.05, minsplit = 5L), maxit = 5L, 
    nParticles = 10L)
  tr1 = tuneParams(makeLearner("classif.rpart"), multiclass.task, res,
    par.set = ps1, control = ctrl1)

  ps2 = makeParamSet(
    makeNumericVectorParam("cutoff", lower = 0.0001, upper = 1, len = 3,
      trafo = function(x) x / (1.1*sum(x))),
    makeIntegerParam("ntree", lower = 100, upper = 500)
  )

  ctrl2 = makeTuneControlPSO(start = list(cutoff = c(1/3, 1/3, 1/3), ntree = 200L),
    maxit = 3L, nParticles = 10L)
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

# -----------------------------------------------------------------------------------------------

test_that("tunePSO with budget", {
  res = makeResampleDesc("CV", iters = 2)
  ps1 = makeParamSet(
    makeNumericParam("cp", lower = 0.001, upper = 1),
    makeIntegerParam("minsplit", lower = 1, upper = 10)
  )

  ctrl = makeTuneControlPSO(start = list(cp = 0.05, minsplit = 5L), maxit = 2L, 
    nParticles = 10L, budget = 50L)
  expect_error(tuneParams(makeLearner("classif.rpart"), multiclass.task, res,
    par.set = ps1, control = ctrl))

  ctrl1 = makeTuneControlPSO(start = list(cp = 0.05, minsplit = 5L), maxit = 3L, 
    nParticles = 10L, budget = 30)
  expect_null(ctrl1$extra.args$type)
  expect_equal(ctrl1$extra.args$maxit, 3L)
  expect_equal(ctrl1$extra.args$nParticles, 10L)

  tr1 = tuneParams(makeLearner("classif.rpart"), multiclass.task, res,
    par.set = ps1, control = ctrl1)
  expect_identical(getOptPathLength(tr1$opt.path), ctrl1$budget)

})