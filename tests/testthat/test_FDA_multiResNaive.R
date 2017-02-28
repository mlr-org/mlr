context("FDA_multiResNaive")

test_that("FDA_multiResNaive", {

  I = 1000  # number of instances
  TL  = 200 # length of each time serie instance
  Ts = replicate(I, rnorm(TL))
  gp = t(as.data.frame(Ts))
  ngp = extractFDAMultiResFeatures (data = gp, curve.lens = c(100, 100), res.level = 3, shift = 0.5)
  expect_true(nrow(ngp) == nrow(gp))
})


test_that("FDA_multiResNaive2", {
  
  I = 1000  # number of instances
  TL  = 200 # length of each time serie instance
  Ts = replicate(I, rnorm(TL))
  gp = t(as.data.frame(Ts))
  ngp = getUniFDAMultiResFeatures(data = gp, res.level = 3, shift = 0.5)
  expect_true(nrow(ngp) == nrow(gp))
})

test_that("FDA_multiResNaive3", {
  df = getTaskData(fuelSubset.task)
  rdesc = getTaskDescription(fuelSubset.task)
  fdf = rdesc$fd.features
  dfn = getMultiFDAMultiResFeatures(data= df, fd.features = fdf)
  expect_true(nrow(df) == nrow(dfn))
})

test_that("FDA_multiResNaive4", {
  df = getTaskData(fuelSubset.task, target.extra = TRUE)
  df = df$data
  rdesc = getTaskDescription(fuelSubset.task)
  fdf = rdesc$fd.features
  dfn = extractFDAMultiResFeatures(data= df, curve.lens = rep(180,2))
  expect_true(nrow(df) == nrow(dfn))
})