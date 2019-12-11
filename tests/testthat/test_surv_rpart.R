context("surv_rpart")

test_that("surv_rpart", {
  requirePackagesOrSkip("rpart", default.method = "load")

  parset.list = list(
    list(),
    list(minsplit = 10, cp = 0.005),
    list(minsplit = 50, cp = 0.05),
    list(minsplit = 50, cp = 0.999),
    list(minsplit = 1, cp = 0.0005)
  )

  old.predicts.list = list()
  old.probs.list = list()

  for (i in seq_along(parset.list)) {
    parset = parset.list[[i]]
    pars = list(formula = surv.formula, data = surv.train)
    pars = c(pars, parset)
    m = do.call(rpart::rpart, pars)
    p = predict(m, newdata = surv.test)
    old.predicts.list[[i]] = p
  }

  testSimpleParsets("surv.rpart", surv.df, surv.target, surv.train.inds,
    old.predicts.list, parset.list)
})
