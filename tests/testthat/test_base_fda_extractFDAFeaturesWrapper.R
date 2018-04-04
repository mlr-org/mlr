context("extactFeaturesWrapper")

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
  tuneParams(lrn, subsetTask(fuelsubset.task, features = 3), cv2, mse, ps2, makeTuneControlRandom(maxit = 2))
})
