context("tuneIrace")

test_that("tuneIrace", {
  rdesc = makeResampleDesc("Holdout", stratify = TRUE, split = 0.1)
  ps1 = makeParamSet(
    makeNumericParam("cp", lower = 0.001, upper = 1),
    makeIntegerParam("minsplit", lower = 1, upper = 10)
  )

  n = 40
  ctrl = makeTuneControlIrace(maxExperiments = n, nbIterations = 2, minNbSurvival = 1)
  tr1 = tuneParams(makeLearner("classif.rpart"), multiclass.task, rdesc, par.set = ps1, control = ctrl)
  expect_true(getOptPathLength(tr1$opt.path) >= 30 && getOptPathLength(tr1$opt.path) <= n)
  expect_true(!is.na(tr1$y))

  # with trafo
  res = makeResampleDesc("Holdout")
  ps2 = makeParamSet(
    makeNumericParam("C", lower = -5, upper = 5, trafo = function(x) 2^x),
    makeNumericParam("sigma", lower = -5, upper = 5, trafo = function(x) 2^x)
  )

  n = 40
  ctrl = makeTuneControlIrace(maxExperiments = n, nbIterations = 2, minNbSurvival = 1)
  tr2 = tuneParams(makeLearner("classif.ksvm"), multiclass.task, rdesc, par.set = ps2, control = ctrl)
  expect_true(getOptPathLength(tr2$opt.path) >= 30 && getOptPathLength(tr2$opt.path) <= n)
  expect_true(!is.na(tr2$y))
})

test_that("tuneIrace works with dependent params", {
  ps = makeParamSet(
    makeDiscreteParam("kernel", values = c("vanilladot", "rbfdot")),
    makeNumericParam("sigma", lower = 1, upper = 2, requires = quote(kernel == "rbfdot"))
  )
  lrn = makeLearner("classif.ksvm")
  rdesc = makeResampleDesc("Holdout")
  ctrl = makeTuneControlIrace(maxExperiments = 40 ,nbIterations = 2, minNbSurvival = 1)
  tr = tuneParams(lrn, multiclass.task, rdesc, par.set = ps, control = ctrl)
  expect_true(getOptPathLength(tr$opt.path) >= 30 && getOptPathLength(tr$opt.path) <= 100)
  expect_true(!is.na(tr$y))

  # another complex example
  ps = makeParamSet(
    makeNumericParam("C", lower = -12, upper = 12, trafo = function(x) 2^x),
    makeDiscreteParam("kernel", values = c("vanilladot", "polydot", "rbfdot")),
    makeNumericParam("sigma", lower = -12, upper = 12, trafo = function(x) 2^x,
      requires = quote(kernel == "rbfdot")),
    makeIntegerParam("degree", lower = 2L, upper = 5L,
      requires = quote(kernel == "polydot"))
  )
  ctrl = makeTuneControlRandom(maxit = 5L)
  rdesc = makeResampleDesc("Holdout")
  res = tuneParams("classif.ksvm", sonar.task, rdesc, par.set = ps, control = ctrl)

})

# we had a bug here
test_that("tuneIrace works with logical params", {
  ps = makeParamSet(
    makeLogicalParam("scaled")
  )
  lrn = makeLearner("classif.ksvm", kernel = "vanilladot")
  rdesc = makeResampleDesc("Holdout", split = 0.3, stratify = TRUE)
  ctrl = makeTuneControlIrace(maxExperiments = 20, nbIterations = 1, minNbSurvival = 1)
  task = subsetTask(multiclass.task, c(1:10, 50:60, 100:110))
  tr = tuneParams(lrn, task, rdesc, par.set = ps, control = ctrl)
  expect_true(getOptPathLength(tr$opt.path) >= 15 && getOptPathLength(tr$opt.path) <= 20)
  expect_true(!is.na(tr$y))

  lrn2 = makeTuneWrapper(lrn, rdesc, par.set = ps, control = ctrl)
  z = holdout(lrn2, task, split = 0.5, stratify = TRUE)
  expect_true(getOptPathLength(tr$opt.path) >= 15 && getOptPathLength(tr$opt.path) <= 20)
  expect_true(!is.na(tr$y))
})

test_that("tuneIrace works with tune.threshold", {
  rdesc = makeResampleDesc("Holdout", stratify = TRUE, split = 0.1)
  ps = makeParamSet(makeIntegerParam("minsplit", lower = 1, upper = 3))

  n = 40
  ctrl = makeTuneControlIrace(maxExperiments = n, nbIterations = 2, minNbSurvival = 1)
  tr = tuneParams("classif.rpart", multiclass.task, rdesc, par.set = ps, control = ctrl)
})

test_that("tuneIrace handles the number of digits", {
  lrn = makeLearner("classif.gbm", predict.type = "prob")
  rdesc = makeResampleDesc(method = "Holdout")
  
  ctrl = makeTuneControlIrace(maxExperiments = 60L)
  ps = makeParamSet(makeNumericParam("shrinkage", lower = 1e-5, upper = 1e-4))
  lrn.tune = makeTuneWrapper(lrn, resampling = rdesc, par.set = ps, 
    control = ctrl, measure = list(mmce), show.info = FALSE)
  res = resample(lrn.tune, task = multiclass.task, rdesc, 
    measures = list(mmce), show.info = FALSE)
  
  ctrl = makeTuneControlIrace(maxExperiments = 120L)
  ps = makeParamSet(
    makeNumericParam("shrinkage", lower = pi * 1e-20, upper = 5.242e12),
    makeIntegerParam("interaction.depth", lower = 1, upper = 10)
  )
  lrn.tune = makeTuneWrapper(lrn, resampling = rdesc, par.set = ps, 
    control = ctrl, measure = list(mmce), show.info = FALSE)
  res = resample(lrn.tune, task = multiclass.task, rdesc, 
    measures = list(mmce), show.info = FALSE)  
})

test_that("tuneIrace uses digits", {
  lrn = makeLearner("classif.gbm", predict.type = "prob")
  rdesc = makeResampleDesc(method = "Holdout")
  
  ctrl = makeTuneControlIrace(maxExperiments = 60L, digits = 5L)
  ps = makeParamSet(makeNumericParam("shrinkage", lower = 1e-5, upper = 1e-4))
  lrn.tune = makeTuneWrapper(lrn, resampling = rdesc, par.set = ps, 
    control = ctrl, measure = list(mmce), show.info = FALSE)
  res = resample(lrn.tune, task = multiclass.task, rdesc, 
    measures = list(mmce), show.info = FALSE)    
  
  ctrl = makeTuneControlIrace(maxExperiments = 60L, digits = 4L)
  ps = makeParamSet(makeNumericParam("shrinkage", lower = 1e-5, upper = 1e-4))
  lrn.tune = makeTuneWrapper(lrn, resampling = rdesc, par.set = ps, 
    control = ctrl, measure = list(mmce), show.info = FALSE)
  expect_error(suppressAll(resample(lrn.tune, task = multiclass.task, rdesc, 
    measures = list(mmce), show.info = FALSE)))
  
  ctrl = makeTuneControlIrace(maxExperiments = 60L, digits = "a")
  ps = makeParamSet(makeNumericParam("shrinkage", lower = 1e-5, upper = 1e-4))
  lrn.tune = makeTuneWrapper(lrn, resampling = rdesc, par.set = ps, 
    control = ctrl, measure = list(mmce), show.info = FALSE)
  expect_error(suppressAll(resample(lrn.tune, task = multiclass.task, rdesc, 
    measures = list(mmce), show.info = FALSE)))
  
  ctrl = makeTuneControlIrace(maxExperiments = 60L, digits = c(6L, 7L))
  ps = makeParamSet(makeNumericParam("shrinkage", lower = 1e-5, upper = 1e-4))
  lrn.tune = makeTuneWrapper(lrn, resampling = rdesc, par.set = ps, 
    control = ctrl, measure = list(mmce), show.info = FALSE)
  expect_error(suppressAll(resample(lrn.tune, task = multiclass.task, rdesc, 
    measures = list(mmce), show.info = FALSE)))
})

test_that("makeTuneControlIrace handles budget parameter", {
  rdesc = makeResampleDesc("Holdout", stratify = TRUE, split = 0.1)
  ps = makeParamSet(makeIntegerParam("minsplit", lower = 1, upper = 3))

  n = 40
  expect_error(makeTuneControlIrace(budget = n, nbIterations = 2, minNbSurvival = 1, maxExperiments = n + 2L))
  expect_error(makeTuneControlIrace(budget = n + 2L, nbIterations = 2, minNbSurvival = 1, maxExperiments = n))

  # check, whether it is ok to provide both arguments as long as they are the same
  ctrl = makeTuneControlIrace(budget = n, nbIterations = 2, minNbSurvival = 1, maxExperiments = n)
  tr1 = tuneParams(makeLearner("classif.rpart"), multiclass.task, rdesc, par.set = ps, control = ctrl)
  expect_true(getOptPathLength(tr1$opt.path) <= n)
})
