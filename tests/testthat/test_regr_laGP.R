context("regr_laGP")

test_that("regr_laGP", {
  requirePackagesOrSkip("laGP", default.method = "load")

  parset.list = list(
    list(),
    list(start = 6, end = 49, close = 50)
  )
  dd = regr.num.df[1:100, ]
  old.predicts.list = list()
  des1 = dd[1:51, setdiff(colnames(dd), regr.num.target)]
  des2 = dd[52:100, setdiff(colnames(dd), regr.num.target)]
  y = dd[1:51, regr.num.target]
  for (i in seq_along(parset.list)) {
    parset = parset.list[[i]]
    pars = list(X = des1[, -regr.num.class.col], Z = y,
      XX = des2[, -regr.num.class.col], verb = 0,
      Xi.ret = FALSE)
    pars = c(pars, parset)
    old.predicts.list[[i]] = do.call(laGP::aGP, pars)$mean
  }
  testSimpleParsets("regr.laGP", dd, regr.num.target, 1:51, old.predicts.list,
    parset.list)
})
