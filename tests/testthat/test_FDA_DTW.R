context("FDA_DTW")

test_that("FDA_DTW", {
  
  I = 10  # number of instances
  TL  = 20 # length of each time serie instance
  Ts = replicate(I, rnorm(TL))
  gp = t(as.data.frame(Ts))
  ngp = getUniDTWFeatures(data = gp)
  expect_true(nrow(ngp) == nrow(gp))
})



