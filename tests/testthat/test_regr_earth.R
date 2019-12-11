context("regr_earth")

test_that("regr_earth", {
  requirePackagesOrSkip("earth", default.method = "load")

  parset.list = list(
    list(),
    list(degree = 2),
    list(penalty = 4)
  )

  old.predicts.list = list()

  for (i in seq_along(parset.list)) {
    parset = parset.list[[i]]
    pars = list(regr.formula, data = regr.train)
    pars = c(pars, parset)
    m = do.call(earth::earth, pars)
    old.predicts.list[[i]] = predict(m, newdata = regr.test)[, 1]
  }

  testSimpleParsets("regr.earth", regr.df, regr.target, regr.train.inds,
    old.predicts.list, parset.list)
})
