context("regr_GPfit")

test_that("regr_GPfit", {
  test_fun = function(x) {
  return(4*x[,1]^2 - 2*x[,2])
  }
  n = 30
  d = 2
  set.seed(getOption("mlr.debug.seed"))
  train.inds = 1:20
  x = lhs::maximinLHS(n,d) 
  y = test_fun(x)
  GPfit.test.df = cbind.data.frame(x, y)
  colnames(GPfit.test.df) = c("x1", "x2", "y")
  m = GPfit::GP_fit(x[train.inds,], y[train.inds])
  p = predict(m, xnew = x[-train.inds,])
  testSimple("regr.GPfit", GPfit.test.df, "y", train.inds, p$Y_hat)
})
