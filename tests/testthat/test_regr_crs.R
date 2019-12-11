context("regr_crs")

test_that("regr_crs", {
  requirePackagesOrSkip("crs", default.method = "load")

  parset.list = list(
    list(nmulti = 1, cv = "none"),
    list(degree = rep(3, 12), nmulti = 1, cv = "none"),
    list(segments = rep(3, 12), nmulti = 1, cv = "none")
  )

  old.predicts.list = list()

  for (i in seq_along(parset.list)) {
    parset = parset.list[[i]]
    pars = list(regr.formula, data = regr.train)
    pars = c(pars, parset)
    suppressWarnings({
      m = do.call(crs::crs, pars)
    })
    # suppressed warnings:
    # some 'x' values beyond boundary knots may cause ill-conditioned bases
    # NaNs produced
    pred = suppressWarnings(predict(m, newdata = regr.test))
    attr(pred, "lwr") = NULL
    attr(pred, "upr") = NULL
    old.predicts.list[[i]] = pred
  }

  suppressWarnings(testSimpleParsets("regr.crs", regr.df, regr.target,
    regr.train.inds, old.predicts.list, parset.list))
})
