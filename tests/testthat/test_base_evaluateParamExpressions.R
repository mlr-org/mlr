context("evaluate param expressions")

test_that("expressions in learners", {
  ## expressions within 'pre-defined' learners
  ## (1) expressions within default of parameter sets
  lrn1 = makeLearner("classif.__mlrmocklearners__7")
  dict = getTaskDictionary(task = binaryclass.task)
  lrn2 = evaluateParamExpressions(obj = lrn1, dict = dict)
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
  dict = getTaskDictionary(task = binaryclass.task)
  lrn2 = evaluateParamExpressions(obj = lrn1, dict = dict)
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
  dict = getTaskDictionary(task = binaryclass.task)
  ps2 = evaluateParamExpressions(obj = ps1, dict = dict)

  ## expressions within parameter sets
  expect_equal(ps2$pars$C$lower, 2L)
  expect_equal(ps2$pars$C$upper, 208L)
  expect_equal(ps2$pars$sigma$values, list(60, 2))
})

test_that("tuning works with expressions", {
  task = multiclass.small.task
  lrn = makeLearner("classif.rpart")
  lrn = makeFilterWrapper(lrn, fw.method = "kruskal.test")
  ps = makeParamSet(
    makeIntegerParam("fw.abs", lower = 1, upper = expression(ceiling(n/2)))
  )
  ctrl = makeTuneControlRandom(maxit = 5)
  res = tuneParams(lrn, task = task, resampling = hout, par.set = ps, control = ctrl)
  res = as.data.frame(res$opt.path)
  expect_integer(res$fw.abs, lower = 1, upper = ceiling(getTaskSize(task)/2), any.missing = FALSE)
})
