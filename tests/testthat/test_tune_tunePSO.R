# -----------------------------------------------------------------------------------------------

context("tunePSO")

# -----------------------------------------------------------------------------------------------

test_that("tunePSO defaults",{
  
  ctrl1 = makeTuneControlPSO()
  expect_equal(ctrl1$extra.args$pso.impl, "SPSO2007")
  expect_equal(ctrl1$extra.args$maxit, 100L)
  expect_true(is.null(ctrl1$budget))
 
  ctrl2 = makeTuneControlPSO(maxit = 5L, pso.impl = "SPSO2011")
  expect_equal(ctrl2$extra.args$pso.impl, "SPSO2011")
  expect_equal(ctrl2$extra.args$maxit, 5L)
  expect_true(is.null(ctrl2$budget))

  #invalid type
  expect_error(makeTuneControlPSO(pso.impl = "Foo"))
  expect_error(makeTuneControlPSO(maxit = "Foo"))

})


# -----------------------------------------------------------------------------------------------

test_that("tunePSO", {
  res = makeResampleDesc("CV", iters = 2L)
  ps1 = makeParamSet(
    makeNumericParam("cp", lower = 0.001, upper = 1),
    makeIntegerParam("minsplit", lower = 1, upper = 10)
  )
  ctrl1 = makeTuneControlPSO(start = list(cp = 0.05, minsplit = 5L),
   maxit = 5L, n.particles = 10L)
  tr1 = tuneParams(makeLearner("classif.rpart"), multiclass.task, res,
    par.set = ps1, control = ctrl1)

  ps2 = makeParamSet(
    makeNumericVectorParam("cutoff", lower = 0.0001, upper = 1, len = 3,
      trafo = function(x) x / (1.1*sum(x))),
    makeIntegerParam("ntree", lower = 100, upper = 500)
  )

  ctrl2 = makeTuneControlPSO(start = list(cutoff = c(1/3, 1/3, 1/3), ntree = 200L),
    maxit = 3L, n.particles = 10L)
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
    n.particles = 10L, budget = 50L)
  expect_error(tuneParams(makeLearner("classif.rpart"), multiclass.task, res,
    par.set = ps1, control = ctrl))

  ctrl1 = makeTuneControlPSO(start = list(cp = 0.05, minsplit = 5L), maxit = 3L,
    n.particles = 10L, budget = 30)
  expect_equal(ctrl1$extra.args$pso.impl, "SPSO2007")
  expect_equal(ctrl1$extra.args$maxit, 3L)
  expect_equal(ctrl1$extra.args$n.particles, 10L)

  tr1 = tuneParams(makeLearner("classif.rpart"), multiclass.task, res,
    par.set = ps1, control = ctrl1)
  expect_identical(getOptPathLength(tr1$opt.path), ctrl1$budget)

})
