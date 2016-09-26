# -----------------------------------------------------------------------------------------------

context("tuneGA")

# -----------------------------------------------------------------------------------------------

test_that("tuneGA defaults",{
  
  ctrl1 = makeTuneControlGA()
  expect_equal(ctrl1$extra.args$maxit, 100L)
  expect_equal(ctrl1$extra.args$pop.size , 50L)
  expect_equal(ctrl1$extra.args$prob.crossover, 0.8)
  expect_equal(ctrl1$extra.args$prob.mutation, 0.1)
  expect_true(is.null(ctrl1$budget))
 
  ctrl2 = makeTuneControlEDA(maxit = 5L, pop.size  = 10L, 
    prob.crossover = 0.4, prob.mutation = 0.2)
  expect_equal(ctrl2$extra.args$maxit, 5L)
  expect_equal(ctrl2$extra.args$pop.size , 10L)
  expect_equal(ctrl2$extra.args$prob.crossover, 0.4)
  expect_equal(ctrl2$extra.args$prob.mutation, 0.2)
  expect_true(is.null(ctrl2$budget))

  #invalid type
  expect_error(makeTuneControlGA(prob.mutation = 4))
  expect_error(makeTuneControlGA(prob.mutation = -4))
  expect_error(makeTuneControlGA(prob.mutation = "foo"))
  expect_error(makeTuneControlGA(prob.crossover = 4))
  expect_error(makeTuneControlGA(prob.crossover = -4))
  expect_error(makeTuneControlGA(prob.crossover = "foo"))
  expect_error(makeTuneControlGA(maxit = "foo"))
  expect_error(makeTuneControlGA(maxit = -1))
  expect_error(makeTuneControlGA(pop.size  = "foo"))
  expect_error(makeTuneControlGA(pop.size  = -1))

})

# -----------------------------------------------------------------------------------------------


test_that("tuneGA", {

  res = makeResampleDesc("CV", iters = 2)
  ps1 = makeParamSet(
    makeNumericParam("cp", lower = 0.001, upper = 1),
    makeIntegerParam("minsplit", lower = 1, upper = 10)
  )
  ctrl1 = makeTuneControlGA(start = list(cp = 0.05, minsplit = 5L),
    maxit = 5, pop.size = 10)
  tr1 = tuneParams(makeLearner("classif.rpart"), multiclass.task, res,
    par.set = ps1, control = ctrl1)

  ps2 = makeParamSet(
    makeNumericVectorParam("cutoff", lower = 0.0001, upper = 1, len = 3,
      trafo = function(x) x / (1.1*sum(x))),
    makeIntegerParam("ntree", lower = 100, upper = 500)
  )

  ctrl2 = makeTuneControlGA(start = list(cutoff = c(1/3, 1/3, 1/3), ntree = 200L),
     maxit = 5, pop.size = 10)
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

test_that("tuneGA with budget", {

  res = makeResampleDesc("CV", iters = 2)
  ps1 = makeParamSet(
    makeNumericParam("cp", lower = 0.001, upper = 1),
    makeIntegerParam("minsplit", lower = 1, upper = 10)
  )

  ctrl = makeTuneControlGA(start = list(cp = 0.05, minsplit = 5L), maxit = 2,
    pop.size = 10, budget = 50)
  expect_error(tuneParams(makeLearner("classif.rpart"), multiclass.task, res,
    par.set = ps1, control = ctrl))

  ctrl1 = makeTuneControlGA(start = list(cp = 0.05, minsplit = 5L), maxit = 3,
    pop.size = 10, budget = 30)
  expect_equal(ctrl1$extra.args$maxit, 3)
  expect_equal(ctrl1$extra.args$pop.size, 10)

  tr1 = tuneParams(makeLearner("classif.rpart"), multiclass.task, res,
    par.set = ps1, control = ctrl1)

  # FIXME: opth.path's size returned is not equal to the budget size,
  # even with the GA's param "run = maxiter".
  # expect_identical(getOptPathLength(tr1$opt.path), ctrl1$budget)
})
