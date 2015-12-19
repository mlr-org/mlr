context("regr_LiblineaRL2L2SVR")

test_that("regr_LiblineaRL2L2SVR", {
  requirePackages("LiblineaR", default.method = "load")

  parset.list = list(
    list(type = 11, svr_eps = 0.01),
    list(type = 12, svr_eps = 0.1),
    list(type = 11, cost = 5L),
    list(type = 12, cost = 5L)
  )

  old.predicts.list = list()
  old.probs.list = list()

  for (i in 1L:length(parset.list)) {
    parset = parset.list[[i]]
    pars = list(data = regr.num.train[, -regr.num.class.col],
      target = regr.num.train[, regr.num.target])
    pars = c(pars, parset)
    set.seed(getOption("mlr.debug.seed"))
    m = do.call(LiblineaR::LiblineaR, pars)
    set.seed(getOption("mlr.debug.seed"))
    p = predict(m, newx = regr.num.test[, -regr.num.class.col])
    old.predicts.list[[i]] = p$predictions
  }
  parset.list = list(
    list(svr_eps = 0.01),
    list(type = 12, svr_eps = 0.1),
    list(cost = 5L),
    list(type = 12, cost = 5L)
  )

  testSimpleParsets("regr.LiblineaRL2L2SVR", regr.num.df, regr.num.target,
    regr.num.train.inds, old.predicts.list, parset.list)

})
