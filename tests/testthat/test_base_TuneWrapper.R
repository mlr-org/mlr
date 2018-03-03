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
  rdesc = makeResampleDesc("Holdout", split = 0.3)
  ctrl = makeTuneControlRandom(maxit = 1L)
  ps = makeParamSet(makeNumericParam("s", lower = 0.001, upper = 0.1))
  tw = makeTuneWrapper(lrn, rdesc, par.set = ps, control = ctrl)
  # this resulted in an error as "s" was not passed to predict
  res = resample(tw, binaryclass.task, rdesc)
  expect_class(res, "ResampleResult")
})

test_that("TuneWrapper uses tune.threshold", {
  lrn = makeLearner("classif.lda", predict.type = "prob")
  rdesc = makeResampleDesc("Holdout")
  costs = matrix(c(0, 5, 1, 0), 2)
  colnames(costs) = rownames(costs) = getTaskDesc(binaryclass.task)$class.levels
  mm = makeCostMeasure(id = "costs", costs = costs, best = 0, worst = 5)
  ps = makeParamSet(makeDiscreteParam("method", "moment"))
  ctrl = makeTuneControlGrid(tune.threshold = TRUE)
  lrn = makeTuneWrapper(lrn, resampling = rdesc, measures = mm, par.set = ps, control = ctrl)
  m = train(lrn, binaryclass.task)
  p = predict(m, binaryclass.task)
  expect_true(!all(p$threshold == 0.5))
  r = resample(lrn, binaryclass.task, resampling = rdesc)
  expect_true(!all(r$pred$threshold == 0.5))
})


test_that("TuneWrapper works with getTuneResult and getNestedTuneResults", {
  inner = makeResampleDesc("Holdout")
  outer = makeResampleDesc("CV", iters = 2)
  ps1 = makeParamSet(makeDiscreteParam(id = "C", values = c(1, 0.000001)))
  lrn1a = makeLearner("classif.ksvm")
  lrn2 = makeTuneWrapper(lrn1a, resampling = inner, par.set = ps1, control = makeTuneControlGrid())
  r = resample(lrn2, binaryclass.task, outer, measures = mlr::mmce, extract = getTuneResult)
  xs = getNestedTuneResultsX(r)
  expect_equal(colnames(xs), "C")
  expect_equal(nrow(xs), 2)
  opdf = getNestedTuneResultsOptPathDf(r)
  expect_true(all(c("iter", "C", "mmce.test.mean") %in% colnames(opdf)))
  expect_equal(nrow(opdf), 4)

  # check trafo arg
  ps2 = makeParamSet(makeNumericParam(id = "C", lower = -2, upper = 2,
    trafo = function(x) 2^x))
  lrn2 = makeTuneWrapper(lrn1a, resampling = inner, par.set = ps2,
    control = makeTuneControlGrid())
  r = resample(lrn2, binaryclass.task, outer, measures = mlr::mmce,
    extract = getTuneResult)
  opdf = getNestedTuneResultsOptPathDf(r, trafo = TRUE)
  expect_true(all(c("iter", "C", "mmce.test.mean") %in% colnames(opdf)))
  expect_equal(nrow(opdf), 20)
  expect_equal(opdf$C, rep(2^seq(-2, 2, length.out = 10), 2))
})


test_that("TuneWrapper works with nested sampling and threshold tuning, cf. issue 242", {
  rdesc = makeResampleDesc("Holdout")
  ctrl = makeTuneControlGrid(tune.threshold = TRUE, tune.threshold.args = list(nsub = 2L))
  ps = makeParamSet(
    makeDiscreteParam("C", 2^ (-1))
  )
  lrn1 = makeLearner("classif.ksvm", predict.type = "prob")
  lrn2 = makeTuneWrapper(lrn1, resampling = rdesc, measures = list(ber, mmce),
    par.set = ps, control = ctrl, show.info = FALSE)
  r = resample(lrn2, iris.task, rdesc, measures = mmce)
  expect_identical(sort(names(r$pred$threshold)), c("setosa", "versicolor", "virginica"))
})

test_that("TuneWrapper with glmnet (#958)", {
  lrn = makeLearner("classif.glmnet", predict.type = "response")
  lrn2 = makeTuneWrapper(lrn, resampling = makeResampleDesc("Holdout"),
    par.set = makeParamSet(makeNumericLearnerParam(id = "alpha", default = 1, lower = 0, upper = 1)),
    control = makeTuneControlRandom())
  mod = train(lrn2, multiclass.task)
  pred = predict(mod, multiclass.task)
  expect_error(pred, NA)
})

