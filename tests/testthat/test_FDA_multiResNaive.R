context("FDA_multiResNaive")

test_that("FDA_multiResNaive1", {

  I = 1000  # number of instances
  TL  = 200 # length of each time serie instance
  Ts = replicate(I, rnorm(TL))
  gp = t(as.data.frame(Ts))
  ngp = getMultiResFeatObsCustomSeg(data = gp, curve.lens = c(100, 100), res.level = 3, shift = 0.5)
  expect_true(nrow(ngp) == nrow(gp))
})


test_that("FDA_multiResNaive2", {

  I = 1000  # number of instances
  TL  = 200 # length of each time serie instance
  Ts = replicate(I, rnorm(TL))
  gp = t(as.data.frame(Ts))
  ngp = getMultiResFeatObs(data = gp, res.level = 3, shift = 0.5)
  expect_true(nrow(ngp) == nrow(gp))
})

test_that("FDA_multiResNaive3", {
  df = getTaskData(fuelsubset.task)
  rdesc = getTaskDesc(fuelsubset.task)
  fdf = rdesc$fd.features
  dfn = getMultiFDAMultiResFeatures(data= df, fd.features = fdf)
  expect_true(nrow(df) == nrow(dfn))
})

test_that("FDA_multiResNaive4", {
  df = getTaskData(fuelsubset.task, target.extra = TRUE)
  df = df$data
  rdesc = getTaskDesc(fuelsubset.task)
  fdf = rdesc$fd.features
  dfn = getMultiResFeatObsCustomSeg(data= df, curve.lens = rep(180,2))
  expect_true(nrow(df) == nrow(dfn))
})

test_that("FDA_multiResNaive5_singleChannel", {
  df = getTaskData(fuelsubset.task)
  rdesc = getTaskDesc(fuelsubset.task)
  fdf = rdesc$fd.features
  dfn = getFDAMultiResFeatures(data= df, target = rdesc$target, include.target = FALSE, res.level = 3L, shift = 0.5, curve.lens = c(50, 85))
  expect_true(nrow(df) == nrow(dfn))
  dfn = getFDAMultiResFeatures(data= df, target = rdesc$target, include.target = FALSE, res.level = 3L, shift = 0.5) # without curve.lens
  expect_true(nrow(df) == nrow(dfn))
})

test_that("FDA_multiResNaive6_task", {
  task = trafoFDATaskToClassifTask(fuelsubset.task, method = "multiRes", pars = list(res.level = 5, shift = 0.3, curve.lens = c(100,34)))  # the sum must be smaller than single channel
  expect_true(getTaskSize(task) == getTaskSize(fuelsubset.task))
})

test_that("FDA_multiResNaive7_task", {
  task = trafoFDATaskToClassifTask(fuelsubset.task, method = "multiRes", pars = list(res.level = 5, shift = 0.3, list(NIR = list(curve.lens = c(100,101)), UVVIS = list(curve.lens = c(100,101)) )))
  expect_true(getTaskSize(task) == getTaskSize(fuelsubset.task))
})
