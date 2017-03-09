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
    makeNumericParam("C", lower = expression(k), upper = expression(n.task), trafo = function(x) 2^x),
    makeDiscreteParam("sigma", values = expression(list(p, k, n)))
  )
  dict = getTaskDictionary(task = binaryclass.task)
  ps2 = evaluateParamExpressions(obj = ps1, dict = dict)

  ## expressions within parameter sets
  expect_equal(ps2$pars$C$lower, 2L)
  expect_equal(ps2$pars$C$upper, 208L)
  expect_equal(ps2$pars$sigma$values, list(60, 2, 208))
})

test_that("tuning works with expressions", {
  task = multiclass.small.task
  lrn = makeLearner("classif.rpart")
  lrn = makeFilterWrapper(lrn, fw.method = "kruskal.test")
  lrn = makeDownsampleWrapper(lrn, dw.perc = 0.5)
  ps = makeParamSet(
    makeIntegerParam("fw.abs", lower = expression(p-1), upper = expression(p)),
    makeIntegerParam("minsplit", lower = expression(n-1), upper = expression(n)),
    makeIntegerParam("maxdepth", lower = expression(p-2), upper = expression(p-1))
  )
  ctrl = makeTuneControlRandom(maxit = 5)
  res.inst = makeResampleInstance(task = task, desc = hout)
  n.task = getTaskSize(task)
  n = ceiling(length(res.inst$train.inds[[1]]) * 0.5)
  p = getTaskNFeats(task)
  res = tuneParams(lrn, task = task, resampling = res.inst, par.set = ps, control = ctrl)
  res = as.data.frame(res$opt.path)
  expect_integer(res$fw.abs, lower = p-2, upper = p-1, any.missing = FALSE)
  # After Downsampling and Resamplesplitting the n (training observations) should be according to the resample instance.
  expect_integer(res$minsplit, lower = n-1, upper = n, any.missing = FALSE)
  # After filtering the p should always be smaller then the number of filtered features:
  expect_true(res$maxdepth < res$fw.abs)
})

test_that("expressions work with subsetting", {
  task = multiclass.task
  lrn = makeLearner("classif.rpart", maxdepth = expression(p-1), minbucket = expression(n.task - 20), maxcompete = expression(k), minsplit = expression(n - 10))

  m = train(learner = lrn, task = task, subset = 1L:100L) #leave out viginica
  p = getTaskNFeats(task)
  n.task = getTaskSize(task)
  n = 100L
  k = 3L
  expect_equal(m$learner$par.vals$maxdepth, p - 1)
  expect_equal(m$learner$par.vals$minbucket, n.task - 20)
  expect_equal(m$learner$par.vals$maxcompete,  3L)
  expect_equal(m$learner$par.vals$minsplit,  n - 10)

  # subsetting inside a wrapper
  p = 2
  lrn2 = makeFilterWrapper(lrn, fw.method = "information.gain", fw.abs = p)
  lrn2 = makeDownsampleWrapper(lrn2, dw.perc = 0.9)
  m2 = train(learner = lrn2, task = task, subset = 1L:100L)
  m2 = getLeafModel(m2)
  expect_equal(m2$learner$par.vals$maxdepth, p - 1)
  expect_equal(m2$learner$par.vals$minbucket, 0.9 * n.task - 20)
  expect_equal(m2$learner$par.vals$maxcompete,  3L)
  expect_equal(m2$learner$par.vals$minsplit,  0.9 * n - 10)
})
