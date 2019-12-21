context("regr_rsm")

test_that("regr_rsm", {
  requirePackagesOrSkip("rsm", default.method = "load")

  data = regr.df[, c("b", "lstat", "medv")]
  pars = list(medv ~ FO(b, lstat), data = data[regr.train.inds, ])
  m = do.call(rsm::rsm, pars)
  p = predict(m, newdata = regr.test)

  testSimple("regr.rsm", data, regr.target, regr.train.inds, p)
})
