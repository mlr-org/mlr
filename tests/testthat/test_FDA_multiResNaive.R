context("FDA_multiResNaive")

test_that("FDA_multiResNaive", {

  I = 1000  # number of instances
  TL  = 200 # length of each time serie instance
  Ts = replicate(I, rnorm(TL))
  gp = t(as.data.frame(Ts))
  ngp = extractMultiResFeatures (data = gp, curve.lens = c(100, 100), res.level = 3, shift = 0.5)
  expect_true(nrow(ngp) == nrow(gp))
})
