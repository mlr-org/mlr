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
