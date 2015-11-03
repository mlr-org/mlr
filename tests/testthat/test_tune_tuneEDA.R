# -----------------------------------------------------------------------------------------------
# -----------------------------------------------------------------------------------------------

library(copulaedas)
context("tuneEDA")


# -----------------------------------------------------------------------------------------------
# -----------------------------------------------------------------------------------------------

test_that("tuneControlEDA parameters setting", {

  # Testing default hyper-parameter values
  ctrl0 = makeTuneControlEDA()
  expect_match(ctrl0$extra.args$type, "UMDA")
  expect_equal(ctrl0$extra.args$popSize, 100)
  expect_equal(ctrl0$extra.args$maxit, 50)
  
  ctrl1 = makeTuneControlEDA(type = "GCEDA", maxit = 10, popSize = 5)
  expect_match(ctrl1$extra.args$type, "GCEDA")
  expect_equal(ctrl1$extra.args$popSize, 5)
  expect_equal(ctrl1$extra.args$maxit, 10)
  
})

# -----------------------------------------------------------------------------------------------
# -----------------------------------------------------------------------------------------------

test_that("tuneUMDA", {
  res = makeResampleDesc("CV", iters = 2)
  ps1 = makeParamSet(
    makeNumericParam("cp", lower = 0.001, upper = 1),
    makeIntegerParam("minsplit", lower = 1, upper = 10)
  )
  ctrl1 = makeTuneControlEDA(start = list(cp = 0.05, minsplit = 5L), maxit = 5, popSize=10L)
  tr1 = tuneParams(makeLearner("classif.rpart"), multiclass.task, res,
    par.set = ps1, control = ctrl1)

  ps2 = makeParamSet(
    makeNumericVectorParam("cutoff", lower = 0.0001, upper = 1, len = 3,
      trafo = function(x) x / (1.1*sum(x))),
    makeIntegerParam("ntree", lower = 100, upper = 500)
  )

  ctrl2 = makeTuneControlEDA(start = list(cutoff = c(1/3, 1/3, 1/3), ntree = 200L),
    maxit = 5, popSize = 10L)
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

# test GCEDA
test_that("tuneGCEDA", {
  res = makeResampleDesc("CV", iters = 2)
  ps1 = makeParamSet(
    makeNumericParam("cp", lower = 0.001, upper = 1),
    makeIntegerParam("minsplit", lower = 1, upper = 10)
  )
  ctrl1 = makeTuneControlEDA(start = list(cp = 0.05, minsplit = 5L), type = "GCEDA", maxit = 5, popSize=10L)
  tr1 = tuneParams(makeLearner("classif.rpart"), multiclass.task, res,
    par.set = ps1, control = ctrl1)

  ps2 = makeParamSet(
    makeNumericVectorParam("cutoff", lower = 0.0001, upper = 1, len = 3,
      trafo = function(x) x / (1.1*sum(x))),
    makeIntegerParam("ntree", lower = 100, upper = 500)
  )

  ctrl2 = makeTuneControlEDA(start = list(cutoff = c(1/3, 1/3, 1/3), ntree = 200L),
    type = "GCEDA", maxit = 5, popSize = 10L)
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

# test CVEDA
test_that("tuneCVEDA", {
  res = makeResampleDesc("CV", iters = 2)
  ps1 = makeParamSet(
    makeNumericParam("cp", lower = 0.001, upper = 1),
    makeIntegerParam("minsplit", lower = 1, upper = 10)
  )
  ctrl1 = makeTuneControlEDA(start = list(cp = 0.05, minsplit = 5L), type = "CVEDA", maxit = 5, popSize=10L)
  tr1 = tuneParams(makeLearner("classif.rpart"), multiclass.task, res,
    par.set = ps1, control = ctrl1)

  ps2 = makeParamSet(
    makeNumericVectorParam("cutoff", lower = 0.0001, upper = 1, len = 3,
      trafo = function(x) x / (1.1*sum(x))),
    makeIntegerParam("ntree", lower = 100, upper = 500)
  )

  ctrl2 = makeTuneControlEDA(start = list(cutoff = c(1/3, 1/3, 1/3), ntree = 200L),
    type = "CVEDA", maxit = 5, popSize = 10L)
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

# test DVEDA
test_that("tuneDVEDA", {
  res = makeResampleDesc("CV", iters = 2)
  ps1 = makeParamSet(
    makeNumericParam("cp", lower = 0.001, upper = 1),
    makeIntegerParam("minsplit", lower = 1, upper = 10)
  )
  ctrl1 = makeTuneControlEDA(start = list(cp = 0.05, minsplit = 5L), type = "DVEDA", maxit = 5, popSize=10L)
  tr1 = tuneParams(makeLearner("classif.rpart"), multiclass.task, res,
    par.set = ps1, control = ctrl1)

  ps2 = makeParamSet(
    makeNumericVectorParam("cutoff", lower = 0.0001, upper = 1, len = 3,
      trafo = function(x) x / (1.1*sum(x))),
    makeIntegerParam("ntree", lower = 100, upper = 500)
  )

  ctrl2 = makeTuneControlEDA(start = list(cutoff = c(1/3, 1/3, 1/3), ntree = 200L),
    type = "DVEDA", maxit = 5, popSize = 10L)
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

test_that("tuneEDA with budget", {
  res = makeResampleDesc("CV", iters = 2)
  ps1 = makeParamSet(
    makeNumericParam("cp", lower = 0.001, upper = 1),
    makeIntegerParam("minsplit", lower = 1, upper = 10)
  )
  expect_error((makeTuneControlEDA(start = list(cp = 0.05, minsplit = 5L), maxit = 5, 
    popSize = 5L, budget = 8)))
  ctrl1 = makeTuneControlEDA(start = list(cp = 0.05, minsplit = 5L), maxit = 3, 
    popSize = 10L, budget = 30)
  tr1 = tuneParams(makeLearner("classif.rpart"), multiclass.task, res,
    par.set = ps1, control = ctrl1)
  expect_identical(getOptPathLength(tr1$opt.path), ctrl1$budget)
})

# -----------------------------------------------------------------------------------------------
# -----------------------------------------------------------------------------------------------