context("FDA_FeatWavelets")

test_that("FDA_FeatWavelets", {
  #gp = data.frame(V1  = t(1:4))
  gp = data.frame(V1 = 1:5, V2 = 12:16, V3 = 3:7, V4 = 4:8)
  
  wavelets.gp = extractFDAFeatWavelets(data = gp, filter = "d2")
  wavelets.ref.gp = extractFDAFeatWavelets(data = gp, filter = "d4", boundary = "reflection")
  expect_equal(nrow(wavelets.gp), nrow(gp))
  expect_equal(nrow(wavelets.ref.gp), nrow(gp))
  # Too many vanishing moments (support width) expected
  expect_error(extractFDAFeatWavelets(data = gp, filter = "d10"))
})
