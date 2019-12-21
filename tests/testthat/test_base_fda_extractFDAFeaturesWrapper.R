context("extactFDAFeaturesWrapper")

test_that("extractFDAFeaturesWrapper", {
  methods = list("UVVIS" = extractFDAMultiResFeatures(), "NIR" = extractFDAFourier())
  lrn = makeExtractFDAFeatsWrapper("regr.rpart", feat.methods = methods)
  expect_class(lrn, "extractFDAFeatsWrapper")

  mod = train(lrn, fuelsubset.task)
  expect_class(mod, "PreprocModel")

  prd = predict(mod, fuelsubset.task)
  expect_data_frame(prd$data)
  expect_numeric(prd$data$response, lower = 0, upper = Inf)

  res = resample(lrn, fuelsubset.task, cv2)
  expect_class(res, "ResampleResult")
})

test_that("extractFDAFeaturesWrapper ParSet Works", {
  methods = list("NIR" = extractFDAFourier())
  lrn = makeExtractFDAFeatsWrapper("regr.rpart", feat.methods = methods)
  ps = getLearnerParamSet(lrn)

  # Check whether all Ids are contained in the resulting param set
  ps.rpart = getLearnerParamSet(makeLearner("regr.rpart"))
  expect_subset(getParamIds(ps.rpart), getParamIds(ps))
  expect_subset(getParamIds(methods$NIR$par.set), getParamIds(ps))

  ps2 = makeParamSet(
    makeDiscreteParam("trafo.coeff", values = c("phase", "amplitude")),
    makeIntegerParam("minsplit", lower = 1, upper = 30))
  mod = tuneParams(lrn, subsetTask(fuelsubset.task, features = 3), cv2, mse, ps2, makeTuneControlRandom(maxit = 2))
})

test_that("extractFDAFeaturesWrapper ParSet Works II", {
  methods = list("fd1" = extractFDAFourier())
  lrn = makeExtractFDAFeatsWrapper("classif.xgboost", feat.methods = methods)
  ps = getLearnerParamSet(lrn)

  # Check whether all Ids are contained in the resulting param set
  ps.xgboost = getLearnerParamSet(makeLearner("classif.xgboost"))
  expect_subset(getParamIds(ps.xgboost), getParamIds(ps))
  expect_subset(getParamIds(methods$fd1$par.set), getParamIds(ps))

  ps2 = makeParamSet(makeDiscreteParam("trafo.coeff", values = c("phase", "amplitude")))
  df = getTaskData(fuelsubset.task, functionals.as = "matrix")[, c("heatan", "UVVIS")]
  colnames(df) = c("target", "fd1")
  df$target = as.factor(round(df$target / 10, 0))
  mod = tuneParams(lrn, makeClassifTask(data = df, target = "target"), cv2, acc, ps2, makeTuneControlGrid(resolution = 2L))
})

test_that("extractFDAFeaturesWrapper ParSet Works III", {
  methods = list("fd" = extractFDAWavelets())
  lrn = makeExtractFDAFeatsWrapper("classif.rpart", feat.methods = methods)
  ps = makeParamSet(
    makeNumericParam("cp", lower = 0.01, upper = 0.3),
    makeDiscreteParam("filter", values = c("la8", "haar"))
  )
  lrn = makeTuneWrapper(learner = lrn, resampling = cv2, measure = acc, par.set = ps, control = makeTuneControlMBO(budget = 5L))
  mod = train(lrn, subsetTask(gunpoint.task, subset = 2:10))
  expect_class(mod, "TuneModel")
})

test_that("extractFDAFeaturesWrapper works for dtwkernel", {
  methods = list("fd" = extractFDADTWKernel(n.refs = 0.7))
  lrn = makeExtractFDAFeatsWrapper("classif.rpart", feat.methods = methods)
  ps = makeParamSet(
    makeNumericParam("cp", lower = 0.01, upper = 0.3),
    makeNumericParam("n.refs", lower = 0.01, upper = 0.05)
  )
  lrn = makeTuneWrapper(learner = lrn, resampling = cv2, measure = acc, par.set = ps, control = makeTuneControlRandom(budget = 2L))
  mod = train(lrn, subsetTask(gunpoint.task, subset = 2:30))
  expect_class(mod, "TuneModel")
})
