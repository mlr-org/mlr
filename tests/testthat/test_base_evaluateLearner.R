context("evaluate expressions")

test_that("expressions in learners", {
  ## expressions within parameter sets
  lrn1 = makeLearner("classif.randomForest")
  lrn2 = evaluateLearner(lrn = lrn1, task = binaryclass.task)
  x1 = lrn1$par.set$pars$mtry$default
  x2 = lrn2$par.set$pars$mtry$default
  expect_true(is.expression(x1))
  expect_true(!is.expression(x2))
  expect_equal(x2, floor(sqrt(ncol(binaryclass.df))))
  
  x1 = lrn1$par.set$pars$classwt$len
  x2 = lrn2$par.set$pars$classwt$len
  expect_true(is.expression(x1))
  expect_true(!is.expression(x2))
  expect_equal(x2, 2)
  
  x1 = lrn1$par.set$pars$cutoff$len
  x2 = lrn2$par.set$pars$cutoff$len
  expect_true(is.expression(x1))
  expect_true(!is.expression(x2))
  expect_equal(x2, 2)
  
  ## expressions within hyperparameters
  lrn1 = makeLearner("classif.rpart", minsplit = expression(k * p))
  lrn2 = evaluateLearner(lrn = lrn1, task = binaryclass.task)
  x1 = lrn1$par.vals$minsplit
  x2 = lrn2$par.vals$minsplit
  expect_true(is.expression(x1))
  expect_true(!is.expression(x2))
  expect_equal(lrn2$par.vals$minsplit, 2 * getTaskNFeats(binaryclass.task))
})

test_that("expressions in parameter sets", {
  ps1 = makeParamSet(
    makeNumericParam("C", lower = expression(a), upper = expression(b), trafo = function(x) 2^x),
    makeDiscreteParam("sigma", values = 2^c(-1, 1)),
    makeDiscreteParam("kernel", values = expression(list(e, f)))
  )
  ps2 = evaluateParset(par.set = ps1, task = binaryclass.task,
    dict = list(a = -2L, b = 3L, e = "rbfdot", f = "laplacedot"))
  
  ## expressions within parameter sets
  expect_equal(ps2$pars$C$lower, -2L)
  expect_equal(ps2$pars$C$upper, 3L)
  expect_equal(ps2$pars$kernel$values, list(rbfdot = "rbfdot", laplacedot = "laplacedot"))
})
