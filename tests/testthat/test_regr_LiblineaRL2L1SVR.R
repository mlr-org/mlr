context("regr_LiblineaRL2L1SVR")

test_that("regr_LiblineaRL2L1SVR", {
  requirePackagesOrSkip("LiblineaR", default.method = "load")

  parset.list = list(
    list(),
    list(cost = 5, epsilon = 0.01),
    list(cost = 5, epsilon = 0.1),
    list(cost = 2, epsilon = 0.01),
    list(cost = 2, epsilon = 0.1),
    list(svr_eps = 0.5)
  )

  old.predicts.list = list()
  old.probs.list = list()

  for (i in seq_along(parset.list)) {
    parset = parset.list[[i]]
    pars = list(data = regr.num.train[, -regr.num.class.col],
      target = regr.num.train[, regr.num.target], type = 13L)
    pars = c(pars, parset)
    set.seed(getOption("mlr.debug.seed"))
    # suppressed warnings: "No value provided for svr_eps. Using default of 0.1!
    m = suppressWarnings(do.call(LiblineaR::LiblineaR, pars))
    p = predict(m, newx = regr.num.test[, -regr.num.class.col])
    old.predicts.list[[i]] = p$predictions
  }

  testSimpleParsets("regr.LiblineaRL2L1SVR", regr.num.df, regr.num.target,
    regr.num.train.inds, old.predicts.list, parset.list)
})
