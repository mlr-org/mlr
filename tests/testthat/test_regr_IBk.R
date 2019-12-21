context("regr_IBk")

test_that("regr_IBk", {
  requirePackagesOrSkip("RWeka", default.method = "load")

  parset.list = list(
    list(),
    list(K = 2)
  )

  old.predicts.list = list()

  for (i in seq_along(parset.list)) {
    parset = parset.list[[i]]
    ctrl = do.call(RWeka::Weka_control, parset)
    pars = list(regr.formula, data = regr.train)
    pars = c(pars, parset)
    m = RWeka::IBk(regr.formula, regr.train, control = ctrl)
    p = predict(m, newdata = regr.test)
    old.predicts.list[[i]] = p
  }

  testSimpleParsets("regr.IBk", regr.df, regr.target, regr.train.inds,
    old.predicts.list, parset.list)
})
