context("TuneWrapper")

test_that("TuneWrapper", {
  # FIXME check opt. parameter is same with tune and tune.wrapper
  outer = makeResampleDesc("Holdout")
  inner = makeResampleDesc("CV", iters = 2)

  ps1 = makeParamSet(makeDiscreteParam(id = "C", values = c(1, 0.000001)))
  lrn1a = makeLearner("classif.ksvm")
  lrn2 = makeTuneWrapper(lrn1a, resampling = inner, par.set = ps1, control = makeTuneControlGrid())

  m = train(lrn2, task = multiclass.task)

  or = m$learner.model$opt.result
  expect_equal(or$x, list(C = 1))

  p = predict(m, task = multiclass.task)
  expect_true(!any(is.na(p$data$response)))

  ps2 = makeParamSet(
    makeNumericParam(id = "C", lower = -5, upper = 5, trafo = function(x) 2^x),
    makeNumericParam(id = "epsilon", lower = 0.1, upper = 1),
    makeNumericParam(id = "sigma", lower = -5, upper = 5, trafo = function(x) 2^x)
  )
  lrn1b = makeLearner("regr.ksvm")
  lrn2 = makeTuneWrapper(lrn1b, resampling = inner, par.set = ps2, control =
    makeTuneControlGenSA(start = list(C = 0, epsilon = 0.1, sigma = 0), max.call = 5))

  m = train(lrn2, task = regr.task)
  or = m$learner.model$opt.result
  expect_equal(getOptPathLength(or$opt.path), 5)
  expect_true(!any(is.na(as.data.frame(or$opt.path)$mse.test.mean)))


  # check that predict.type is taken from base learner
  lrn1 = makeLearner("classif.ksvm", predict.type = "prob")
  lrn2 = makeTuneWrapper(lrn1, resampling = makeResampleDesc("Holdout"), par.set = ps1, control = makeTuneControlGrid())
  expect_equal(lrn2$predict.type, "prob")
  r = resample(lrn2, binaryclass.task, makeResampleDesc("Holdout"), measures = mlr::auc)
  expect_true(!is.na(r$aggr[["auc.test.mean"]]))
})

# see bug in issue 205
test_that("TuneWrapper passed predict hyper pars correctly to base learner", {
  lrn = makeLearner("classif.glmnet", predict.type = "prob")
  rdesc = makeResampleDesc("Holdout", split = 0.2)
  ctrl = makeTuneControlRandom(maxit = 1L)
  ps = makeParamSet(makeNumericParam("s", lower = 0.001, upper = 0.1))
  tw = makeTuneWrapper(lrn, rdesc, par.set = ps, control = ctrl)
  # this resulted in an error as "s" was not passed to predict
  resample(tw, binaryclass.task, rdesc)
})


