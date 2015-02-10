context("regr_laGP")

test_that("regr_laGP", {
  requirePackages("laGP", default.method = "load")
  parset.list = list(
    list(start = 6, end = 50, close = 50)
  )
  dd = regr.df[1:100, c(1:3, 14)]
  old.predicts.list = list()
  des1 = dd[1:50, setdiff(colnames(dd), regr.target)]
  des2 = dd[51:100, setdiff(colnames(dd), regr.target)]
  y = dd[1:50, regr.target]
  for (i in 1:length(parset.list)) {
    parset = parset.list[[i]]
    pars = list(X = des1[, 1:3], Z = y, XX = des2[, 1:3], verb = 0,
                Xi.ret = FALSE)
    pars = c(pars, parset)
    set.seed(getOption("mlr.debug.seed"))
    old.predicts.list[[i]] = do.call(laGP::aGP, pars)$mean
  }
  testSimpleParsets("regr.laGP", dd, regr.target, 1:50, old.predicts.list, parset.list)
})
