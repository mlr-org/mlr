context("FDA_FeatFourier")

test_that("FDA_FeatFourier", {
  gp1 = data.frame(v1 = 1:5, v2 = 2:6, v3 = 3:7, v4 = 4:8)

  fourier.gp = extractFDAFeatFourier(data = gp1, trafo.coeff = "phase")
  expect_equal(nrow(fourier.gp), nrow(gp1))
  # Phase (arctan(...) in range(-pi/2, pi/2) )
  expect_true(all(fourier.gp < pi / 2 & fourier.gp > - pi / 2))

  fourier.a.gp = extractFDAFeatFourier(data = gp1, trafo.coeff = "amplitude")
  expect_equal(nrow(fourier.a.gp), nrow(gp1))
  # Amplitude sqrt(Re^2 + Im^2) >= 0
  expect_true(all(fourier.a.gp >= 0))

  # Can not have factors
  gp2 = data.frame(v1  =  t(1:4), X1 = as.factor(1))
  expect_error(extractFDAFeatFourier(data = gp2, trafo.coeff = "amplitude"))
})
