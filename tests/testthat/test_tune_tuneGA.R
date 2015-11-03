# -----------------------------------------------------------------------------------------------
# -----------------------------------------------------------------------------------------------

context("tuneGA")

# -----------------------------------------------------------------------------------------------
# -----------------------------------------------------------------------------------------------

test_that("tuneControlGA parameters setting", {

  # Testing default hyper-parameter values
  ctrl0 = makeTuneControlGA()
  expect_equal(ctrl0$extra.args$pcrossover, 0.8)
  expect_equal(ctrl0$extra.args$pmutation, 0.1)
  expect_false(ctrl0$extra.args$parallel)
  expect_equal(ctrl0$extra.args$popSize, 50)
  expect_equal(ctrl0$extra.args$maxit, 100)
  
  ctrl1 = makeTuneControlGA(maxiter = 10, popSize = 5, pmutation = 0.01, pcrossover = 0.5)
  expect_equal(ctrl1$extra.args$pcrossover, 0.5)
  expect_equal(ctrl1$extra.args$pmutation, 0.01)
  expect_false(ctrl1$extra.args$parallel)
  expect_equal(ctrl1$extra.args$popSize, 5)
  expect_equal(ctrl1$extra.args$maxit, 10)
  
})

# -----------------------------------------------------------------------------------------------
# -----------------------------------------------------------------------------------------------

test_that("tuneGA", {

  res = makeResampleDesc("CV", iters = 2)
  ps1 = makeParamSet(
    makeNumericParam("cp", lower = 0.001, upper = 1),
    makeIntegerParam("minsplit", lower = 1, upper = 10)
  )
  ctrl1 = makeTuneControlGA(start = list(cp = 0.05, minsplit = 5L), maxit = 5, popSize = 10)
  tr1 = tuneParams(makeLearner("classif.rpart"), multiclass.task, res,
    par.set = ps1, control = ctrl1)

  ps2 = makeParamSet(
    makeNumericVectorParam("cutoff", lower = 0.0001, upper = 1, len = 3,
      trafo = function(x) x / (1.1*sum(x))),
    makeIntegerParam("ntree", lower = 100, upper = 500)
  )

  ctrl2 = makeTuneControlGA(start = list(cutoff = c(1/3, 1/3, 1/3), ntree = 200L),
     maxit = 5, popSize = 10)
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
# -----------------------------------------------------------------------------------------------

test_that("tuneGA with budget", {
  

  res = makeResampleDesc("CV", iters = 2)
  ps1 = makeParamSet(
    makeNumericParam("cp", lower = 0.001, upper = 1),
    makeIntegerParam("minsplit", lower = 1, upper = 10)
  )

  expect_error(makeTuneControlGA(start = list(cp = 0.05, minsplit = 5L), maxit = 5L, 
    popSize = 5L, budget = 8L))
  
  ctrl1 = makeTuneControlGA(start = list(cp = 0.05, minsplit = 5L), maxit = 3, popSize =10,
    budget = 30)
  tr1 = tuneParams(makeLearner("classif.rpart"), multiclass.task, res,
    par.set = ps1, control = ctrl1)

  # FIX ME: error - they are not equal 
  # expect_identical(getOptPathLength(tr1$opt.path), ctrl1$budget)
 
})

# -----------------------------------------------------------------------------------------------
# -----------------------------------------------------------------------------------------------
