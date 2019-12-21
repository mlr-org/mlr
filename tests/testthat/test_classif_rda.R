context("classif_rda")

test_that("classif_rda", {
  requirePackagesOrSkip("klaR", default.method = "load")

  m = klaR::rda(formula = multiclass.formula, data = multiclass.train)
  p = predict(m, newdata = multiclass.test)$class

  testSimple("classif.rda", multiclass.df, multiclass.target,
    multiclass.train.inds, p)

  parset.list = list(
    list(gamma = 0.1, lambda = 0.1),
    list(gamma = 0.5, lambda = 1),
    list(gamma = 1, lambda = 0)
  )

  old.predicts.list = list()
  old.probs.list = list()

  for (i in seq_along(parset.list)) {
    parset = parset.list[[i]]
    pars = list(formula = multiclass.formula, data = multiclass.train)
    pars = c(pars, parset)
    m = do.call(klaR::rda, pars)
    p = predict(m, newdata = multiclass.test)
    old.predicts.list[[i]] = p$class
    old.probs.list[[i]] = p$posterior
  }

  testSimpleParsets("classif.rda", multiclass.df, multiclass.target,
    multiclass.train.inds, old.predicts.list, parset.list)
  testProbParsets("classif.rda", multiclass.df, multiclass.target,
    multiclass.train.inds, old.probs.list, parset.list)

  tt = klaR::rda
  tp = function(model, newdata) predict(model, newdata)$class

  testCVParsets("classif.rda", multiclass.df, multiclass.target,
    tune.train = tt, tune.predict = tp, parset.list = parset.list)
})
