context("regr_LiblineaRL2L2SVR")

test_that("regr_LiblineaRL2L2SVR", {
  requirePackagesOrSkip("LiblineaR", default.method = "load")

  parset.list1 = list(
    list(type = 11),
    list(type = 11, svr_eps = 0.01),
    list(type = 12, svr_eps = 0.1),
    list(type = 11, cost = 5L),
    list(type = 12, cost = 5L)
  )

  parset.list2 = list(
    list(),
    list(svr_eps = 0.01),
    list(type = 12, svr_eps = 0.1),
    list(cost = 5L),
    list(type = 12, cost = 5L)
  )

  old.predicts.list = list()
  old.probs.list = list()

  for (i in seq_along(parset.list1)) {
    parset = parset.list1[[i]]
    pars = list(data = regr.num.train[, -regr.num.class.col],
      target = regr.num.train[, regr.num.target])
    pars = c(pars, parset)
    set.seed(getOption("mlr.debug.seed"))
    # suppressed warnings: "No value provided for svr_eps. Using default of 0.1"
    m = suppressWarnings(do.call(LiblineaR::LiblineaR, pars))
    p = predict(m, newx = regr.num.test[, -regr.num.class.col])
    old.predicts.list[[i]] = p$predictions
  }

  testSimpleParsets("regr.LiblineaRL2L2SVR", regr.num.df, regr.num.target,
    regr.num.train.inds, old.predicts.list, parset.list2)
})
