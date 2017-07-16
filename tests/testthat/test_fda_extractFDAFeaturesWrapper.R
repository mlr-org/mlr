context("FDA_extactFeatures")
test_that("extractFDAFeaturesWrapper", {
  methods = list("UVVIS" = extractFDAMean(), "NIR" = extractFDAMedian())
  lrn = makeExtractFDAFeatsWrapper("regr.rpart", feat.methods = methods)
  expect_class(lrn, "extractFDAFeatsWrapper")
  mod = train(lrn, fuelsubset.task)
  expect_class(mod, "PreprocModel")
  prd = predict(mod, fuelsubset.task)
  expect_data_frame(prd$data)
  expect_numeric(prd$data$response, lower = 0, upper = Inf)
})


test_that("extractFDAFeaturesWrapper Resampling", {
  methods = list("UVVIS" = extractFDAMean(), "NIR" = extractFDAMinMax())
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

