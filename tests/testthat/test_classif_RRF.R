context("classif_RRF")

test_that("classif_RRF", {
  requirePackages("RRF", default.method = "load")

  parset.list = list(
    list(),
    list(ntree = 50, mtry = 2),
    list(ntree = 50, mtry = 4)
  )

  old.predicts.list = list()
  old.probs.list = list()

  for (i in seq_along(parset.list)) {
    parset = parset.list[[i]]
    pars = list(formula = multiclass.formula, data = multiclass.train)
    pars = c(pars, parset)
    set.seed(getOption("mlr.debug.seed"))
    m = do.call(RRF::RRF, pars)
    p = predict(m, newdata = multiclass.test, type = "response")
    p2 = predict(m, newdata = multiclass.test, type = "prob")
    old.predicts.list[[i]] = p
    old.probs.list[[i]] = p2
  }

  testSimpleParsets("classif.RRF", multiclass.df, multiclass.target,
    multiclass.train.inds, old.predicts.list, parset.list)
  testProbParsets("classif.RRF", multiclass.df, multiclass.target,
    multiclass.train.inds, old.probs.list, parset.list)

  tt = RRF::RRF
  testCVParsets("classif.RRF", multiclass.df, multiclass.target, tune.train = tt,
    parset.list = parset.list)
})
