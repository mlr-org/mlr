context("extractFDAFeatures_multiResNaive")

test_that("extractMultiResFeatures works on data.frame", {
  i = 100 # number of instances
  tl  = 200 # length of each time serie instance
  ts = replicate(i, rnorm(tl))
  gp = t(as.data.frame(ts))
  ngp = extractMultiResFeatures(data = gp, curve.lens = c(100, 100), res.level = 3, shift = 0.5)
  expect_true(nrow(ngp) == nrow(gp))
  expect_true(ncol(ngp) == 20L)
})


test_that("getUniFDAMultiResFeatures works on data.frame", {
  i = 100 # number of instances
  tl  = 200 # length of each time serie instance
  ts = replicate(i, rnorm(tl))
  gp = t(as.data.frame(ts))
  ngp = getUniFDAMultiResFeatures(data = gp, res.level = 3, shift = 0.5)
  expect_true(nrow(ngp) == nrow(gp))
  expect_true(ncol(ngp) == 11L)
})

test_that("get...FDAMultiResFeatures works on data.frame", {
  df = getTaskData(fuelsubset.task, functionals.as = "matrix")

  dfn = extractMultiResFeatures(df, cols = "UVVIS", res.level = 3L, shift = 0.5)
  expect_true(nrow(df) == nrow(dfn))
  expect_true(ncol(dfn) == 9L)

  dfn2 = extractMultiResFeatures(df, cols = "NIR", res.level = 3L, shift = 0.5)
  expect_true(nrow(df) == nrow(dfn2))
  expect_true(ncol(dfn2) == 9L)

  expect_true(!all(dfn == dfn2))

  dfn = extractMultiResFeatures(df, col = "NIR", res.level = 3L, shift = 0.5,
    curve.lens = c(100L, 131L))
  expect_true(nrow(df) == nrow(dfn))
  expect_true(ncol(dfn) == 19L)

  dfn = extractMultiResFeatures(df, col = "NIR", res.level = 3L, shift = 0.5, curve.lens = 231L)
  expect_true(nrow(df) == nrow(dfn))
  expect_true(ncol(dfn) == 9L)
})
