context("regr_plsr")

test_that("regr_plsr", {
  requirePackagesOrSkip("pls", default.method = "load")

  parset.list = list(
    list(),
    list(ncomp = 1),
    list(ncomp = 3, method = "simpls")
  )

  old.predicts.list = list()

  for (i in seq_along(parset.list)) {
    parset = parset.list[[i]]
    pars = list(regr.formula, data = regr.train)
    pars = c(pars, parset)
    m = do.call(pls::plsr, pars)
    old.predicts.list[[i]] = pls:::predict.mvr(m, newdata = regr.test,
      comps = 1:m$ncomp)[, 1]
  }

  testSimpleParsets("regr.plsr", regr.df, regr.target, regr.train.inds,
    old.predicts.list, parset.list)
})
