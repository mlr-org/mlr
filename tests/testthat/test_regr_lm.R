context("regr_lm")

test_that("regr_lm", {
  pars = list(regr.formula, data = regr.train)
  m = do.call(lm, pars)
  p = predict(m, newdata = regr.test)

  testSimple("regr.lm", regr.df, regr.target, regr.train.inds, p)
})
