context("extactFDAFeaturesBsignal")

test_that("extractBsignal features", {
  methods = list("UVVIS" = extractFDABsignal(), "NIR" = extractFDABsignal())
  t = extractFDAFeatures(fuelsubset.task, feat.methods = methods)
  # check output data
  df = getTaskData(t$task)
  expect_is(df, "data.frame")
  expect_equal(nrow(df), 129)
})


