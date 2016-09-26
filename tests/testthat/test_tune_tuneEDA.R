# -----------------------------------------------------------------------------------------------

context("tuneEDA")

# -----------------------------------------------------------------------------------------------

test_that("tuneEDA_defaults", {

  ctrl1 = makeTuneControlEDA()
  expect_equal(ctrl1$extra.args$maxit, 100L)
  expect_equal(ctrl1$extra.args$pop.size, 50L)
  expect_equal(ctrl1$extra.args$eda.impl, "UMDA")
  expect_true(is.null(ctrl1$budget))
 
  ctrl2 = makeTuneControlEDA(maxit = 5L, pop.size = 10L)
  expect_equal(ctrl2$extra.args$maxit, 5L)
  expect_equal(ctrl2$extra.args$pop.size, 10L)
  expect_equal(ctrl2$extra.args$eda.impl, "UMDA")
  expect_true(is.null(ctrl2$budget))
  
  ctrl3 = makeTuneControlEDA(eda.impl = "GCEDA")
  expect_equal(ctrl3$extra.args$maxit, 100L)
  expect_equal(ctrl3$extra.args$pop.size, 50L)
  expect_equal(ctrl3$extra.args$eda.impl, "GCEDA")
  expect_true(is.null(ctrl3$budget))
  
  expect_error(makeTuneControlEDA(eda.impl = "Foo"))
  expect_error(makeTuneControlEDA(maxit = "Foo"))
  expect_error(makeTuneControlEDA(maxit = -1))

})

# -----------------------------------------------------------------------------------------------

test_that("tuneUMDA", {

  res = makeResampleDesc("CV", iters = 2)
  ps1 = makeParamSet(
    makeNumericParam("cp", lower = 0.001, upper = 1),
    makeIntegerParam("minsplit", lower = 1, upper = 10)
  )

  ctrl1 = makeTuneControlEDA(start = list(cp = 0.05, minsplit = 5L), maxit = 5L, pop.size = 10L)
  tr1 = tuneParams(makeLearner("classif.rpart"), multiclass.task, res,
    par.set = ps1, control = ctrl1)
 
  ps2 = makeParamSet(
    makeNumericVectorParam("cutoff", lower = 0.0001, upper = 1, len = 3,
      trafo = function(x) x / (1.1*sum(x))),
    makeIntegerParam("ntree", lower = 100, upper = 500)
  )

  ctrl2 = makeTuneControlEDA(start = list(cutoff = c(1/3, 1/3, 1/3), ntree = 200L),
    maxit = 5, pop.size = 10L)
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

test_that("tuneGCEDA", {
  res = makeResampleDesc("CV", iters = 2)
  ps1 = makeParamSet(
    makeNumericParam("cp", lower = 0.001, upper = 1),
    makeIntegerParam("minsplit", lower = 1, upper = 10)
  )
  ctrl1 = makeTuneControlEDA(start = list(cp = 0.05, minsplit = 5L), eda.impl = "GCEDA", 
    maxit = 5, pop.size = 10L)
  tr1 = tuneParams(makeLearner("classif.rpart"), multiclass.task, res,
    par.set = ps1, control = ctrl1)

  ps2 = makeParamSet(
    makeNumericVectorParam("cutoff", lower = 0.0001, upper = 1, len = 3,
      trafo = function(x) x / (1.1*sum(x))),
    makeIntegerParam("ntree", lower = 100, upper = 500)
  )

  ctrl2 = makeTuneControlEDA(start = list(cutoff = c(1/3, 1/3, 1/3), ntree = 200L),
    eda.impl = "GCEDA", maxit = 5, pop.size = 10L)
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

test_that("tuneCVEDA", {
  res = makeResampleDesc("CV", iters = 2)
  ps1 = makeParamSet(
    makeNumericParam("cp", lower = 0.001, upper = 1),
    makeIntegerParam("minsplit", lower = 1, upper = 10)
  )
  ctrl1 = makeTuneControlEDA(start = list(cp = 0.05, minsplit = 5L), eda.impl = "CVEDA", 
    maxit = 5, pop.size = 10L)
  tr1 = tuneParams(makeLearner("classif.rpart"), multiclass.task, res,
    par.set = ps1, control = ctrl1)

  ps2 = makeParamSet(
    makeNumericVectorParam("cutoff", lower = 0.0001, upper = 1, len = 3,
      trafo = function(x) x / (1.1*sum(x))),
    makeIntegerParam("ntree", lower = 100, upper = 500)
  )

  ctrl2 = makeTuneControlEDA(start = list(cutoff = c(1/3, 1/3, 1/3), ntree = 200L),
    eda.impl = "CVEDA", maxit = 5, pop.size = 10L)
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

test_that("tuneDVEDA", {
  res = makeResampleDesc("CV", iters = 2)
  ps1 = makeParamSet(
    makeNumericParam("cp", lower = 0.001, upper = 1),
    makeIntegerParam("minsplit", lower = 1, upper = 10)
  )
  ctrl1 = makeTuneControlEDA(start = list(cp = 0.05, minsplit = 5L), eda.impl = "DVEDA",
   maxit = 5, pop.size = 10L)
  tr1 = tuneParams(makeLearner("classif.rpart"), multiclass.task, res,
    par.set = ps1, control = ctrl1)
 
  ps2 = makeParamSet(
    makeNumericVectorParam("cutoff", lower = 0.0001, upper = 1, len = 3,
      trafo = function(x) x / (1.1*sum(x))),
    makeIntegerParam("ntree", lower = 100, upper = 500)
  )

  ctrl2 = makeTuneControlEDA(start = list(cutoff = c(1/3, 1/3, 1/3), ntree = 200L),
    eda.impl = "DVEDA", maxit = 5, pop.size = 10L)
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

test_that("tuneEDA with budget", {
  res = makeResampleDesc("CV", iters = 2)
  ps1 = makeParamSet(
    makeNumericParam("cp", lower = 0.001, upper = 1),
    makeIntegerParam("minsplit", lower = 1, upper = 10)
  )

 ctrl = makeTuneControlEDA(start = list(cp = 0.05, minsplit = 5L), maxit = 2L,
    pop.size = 10L, budget = 50L)
  expect_error(tuneParams(makeLearner("classif.rpart"), multiclass.task, res,
    par.set = ps1, control = ctrl))

  ctrl1 = makeTuneControlEDA(start = list(cp = 0.05, minsplit = 5L), maxit = 3,
    pop.size = 10L, budget = 30)
  tr1 = tuneParams(makeLearner("classif.rpart"), multiclass.task, res,
    par.set = ps1, control = ctrl1)
  expect_identical(getOptPathLength(tr1$opt.path), ctrl1$budget)
})
