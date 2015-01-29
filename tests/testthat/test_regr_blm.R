context("regr_blm")

test_that("regr_blm", {
  requirePackages("tgp", default.method = "load")
  parset.list = list(
    list(meanfn = "linear", bprior = "bflat"),
    list(meanfn = "linear", bprior = "bmle"),
    list(meanfn = "constant")
  )
  y = regr.train[, regr.target]
  old.predicts.list = list()
  for (i in 1:length(parset.list)) {
    parset = parset.list[[i]]
    pars = list(X = regr.train[, 1:3], Z = y, verb = 0, pred.n = FALSE)
    pars = c(pars, parset)
    set.seed(getOption("mlr.debug.seed"))
    m = do.call(tgp::blm, pars)

    old.predicts.list[[i]] = predict(m, XX = regr.test[, 1:3], pred.n = FALSE)$ZZ.km
  }
  testSimpleParsets("regr.blm", regr.df[, c(1:3, 14)], regr.target, regr.train.inds, old.predicts.list, parset.list)
})

