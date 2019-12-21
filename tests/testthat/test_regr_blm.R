context("regr_blm")

test_that("regr_blm", {
  requirePackagesOrSkip("tgp", default.method = "load")

  parset.list = list(
    list(meanfn = "linear", bprior = "bflat"),
    list(meanfn = "linear", bprior = "bmle"),
    list(meanfn = "constant")
  )
  y = regr.num.train[, regr.num.target]
  old.predicts.list = list()
  for (i in seq_along(parset.list)) {
    parset = parset.list[[i]]
    pars = list(X = regr.num.train[, -regr.num.class.col], Z = y, verb = 0,
      pred.n = FALSE)
    pars = c(pars, parset)
    m = do.call(tgp::blm, pars)

    old.predicts.list[[i]] = predict(m,
      XX = regr.num.test[, -regr.num.class.col], pred.n = FALSE)$ZZ.km
  }
  testSimpleParsets("regr.blm", regr.num.df, regr.num.target,
    regr.num.train.inds, old.predicts.list, parset.list)
})
