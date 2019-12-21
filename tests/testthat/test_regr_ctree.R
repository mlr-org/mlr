context("regr_ctree")

test_that("regr_ctree", {
  requirePackagesOrSkip("party", default.method = "load")

  parset.list = list(
    list(),
    list(minsplit = 10, mincriterion = 0.005),
    list(minsplit = 50, mincriterion = 0.05),
    list(minsplit = 50, mincriterion = 0.999),
    list(minsplit = 1, mincriterion = 0.0005)
  )

  old.predicts.list = list()

  for (i in seq_along(parset.list)) {
    parset = parset.list[[i]]
    ctrl = do.call(party::ctree_control, parset)
    m = party::ctree(formula = regr.formula, data = regr.train, controls = ctrl)
    p = predict(m, newdata = regr.test, type = "response")[, 1L]
    old.predicts.list[[i]] = p
  }

  testSimpleParsets("regr.ctree", regr.df, regr.target, regr.train.inds,
    old.predicts.list, parset.list)
})
