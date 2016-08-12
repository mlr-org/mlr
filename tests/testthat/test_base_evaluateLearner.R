context("evaluate expressions")

test_that("expressions in learners", {
  ## expressions within 'pre-defined' learners
  ## (1) expressions within default of parameter sets
  lrn1 = makeLearner("classif.__mlrmocklearners__7")
  lrn2 = evaluateLearner(learner = lrn1, task = binaryclass.task)
  x1 = lrn1$par.set$pars$mtry$default
  x2 = lrn2$par.set$pars$mtry$default
  expect_true(is.expression(x1))
  expect_true(!is.expression(x2))
  expect_equal(x2, floor(sqrt(ncol(binaryclass.df))))

  ## (2) expressions within length of parameter sets
  x1 = lrn1$par.set$pars$classwt$len
  x2 = lrn2$par.set$pars$classwt$len
  expect_true(is.expression(x1))
  expect_true(!is.expression(x2))
  expect_equal(x2, 2L)

  ## (3) expressions that go deeper into the task
  x1 = lrn1$par.set$pars$importance$default
  x2 = lrn2$par.set$pars$importance$default
  expect_true(is.expression(x1))
  expect_true(!is.expression(x2))
  expect_equal(x2, binaryclass.task$task.desc$has.blocking)
  
  ## (4) expressions within hyperparameters
  x1 = lrn1$par.vals$minsplit
  x2 = lrn2$par.vals$minsplit
  expect_true(is.expression(x1))
  expect_true(!is.expression(x2))
  expect_equal(x2, ceiling(0.1 * sum(dim(binaryclass.df))))

  ## manually constructed expressions within hyperparams
  lrn1 = makeLearner("classif.rpart", minsplit = expression(k * p))
  lrn2 = evaluateLearner(learner = lrn1, task = binaryclass.task)
  x1 = lrn1$par.vals$minsplit
  x2 = lrn2$par.vals$minsplit
  expect_true(is.expression(x1))
  expect_true(!is.expression(x2))
  expect_equal(lrn2$par.vals$minsplit, 2 * getTaskNFeats(binaryclass.task))
})

test_that("expressions in parameter sets", {
  ps1 = makeParamSet(
    makeNumericParam("C", lower = expression(k), upper = expression(n), trafo = function(x) 2^x),
    makeDiscreteParam("sigma", values = expression(list(p, k)))
  )
  ps2 = evaluateParset(par.set = ps1, task = binaryclass.task)
  
  ## expressions within parameter sets
  expect_equal(ps2$pars$C$lower, 2L)
  expect_equal(ps2$pars$C$upper, 208L)
  expect_equal(ps2$pars$sigma$values, list("60" = 60, "2" = 2))
})
