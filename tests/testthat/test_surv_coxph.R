context("surv_coxph")

test_that("surv_coxph", {
  requirePackagesOrSkip("survival", default.method = "load")

  parset.list = list(
    list(),
    list(iter.max = 1),
    list(iter.max = 10),
    list(iter.max = 50)
  )

  old.predicts.list = list()

  for (i in seq_along(parset.list)) {
    parset = parset.list[[i]]
    pars = list(formula = surv.formula, data = surv.train)
    pars = c(pars, parset)
    m = do.call(survival::coxph, pars)
    p = predict(m, newdata = surv.test, type = "lp")
    old.predicts.list[[i]] = p
  }

  testSimpleParsets("surv.coxph", surv.df, surv.target, surv.train.inds,
    old.predicts.list, parset.list)
})
