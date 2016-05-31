context("regr_gpfit")

test_that("regr_gpfit", {
  test_fun <- function(x) {
  return(4*x[,1]^2 - 2*x[,2])
  }
  n = 30
  d = 2
  set.seed(1)
  train.inds = 1:20
  x = lhs::maximinLHS(n,d) 
  y = test_fun(x)
  gpfit.test.df = cbind.data.frame(x, y)
  colnames(gpfit.test.df) = c("x1", "x2", "y")
  set.seed(getOption("mlr.debug.seed"))
  m = GPfit::GP_fit(x[train.inds,], y[train.inds])
  p = predict(m, xnew = x[-train.inds,])
  testSimple("regr.gpfit", gpfit.test.df, "y", train.inds, p$Y_hat)
})
