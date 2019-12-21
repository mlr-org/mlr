context("regr_GPfit")

test_that("regr_GPfit", {
  testFun = function(x) {
    return(4 * x[, 1]^2 - 2 * x[, 2])
  }
  train.inds = 1:20
  s = seq(0, 1, by = 0.2)
  x = expand.grid(s, s)
  y = testFun(x)
  gpfit.test.df = cbind.data.frame(x, y)
  colnames(gpfit.test.df) = c("x1", "x2", "y")
  m = GPfit::GP_fit(x[train.inds, ], y[train.inds])
  p = predict(m, xnew = x[-train.inds, ])
  testSimple("regr.GPfit", gpfit.test.df, "y", train.inds, p$Y_hat)
})
