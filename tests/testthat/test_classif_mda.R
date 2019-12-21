context("classif_mda")

test_that("classif_mda", {
  requirePackagesOrSkip("mda", default.method = "load")

  parset.list1 = list(
    list(start.method = "lvq"),
    list(start.method = "lvq", subclasses = 2),
    list(start.method = "lvq", subclasses = 3)
  )

  parset.list2 = list(
    list(),
    list(start.method = "lvq", subclasses = 2),
    list(start.method = "lvq", subclasses = 3)
  )

  old.predicts.list = list()
  old.probs.list = list()

  for (i in seq_along(parset.list1)) {
    parset = parset.list1[[i]]
    pars = list(formula = multiclass.formula, data = multiclass.train)
    pars = c(pars, parset)
    set.seed(getOption("mlr.debug.seed"))
    m = do.call(mda::mda, pars)
    p = predict(m, newdata = multiclass.test)
    p2 = predict(m, newdata = multiclass.test, type = "posterior")
    old.predicts.list[[i]] = p
    old.probs.list[[i]] = p2
  }

  testSimpleParsets("classif.mda", multiclass.df, multiclass.target,
    multiclass.train.inds, old.predicts.list, parset.list2)
  testProbParsets("classif.mda", multiclass.df, multiclass.target,
    multiclass.train.inds, old.probs.list, parset.list2)

  tt = mda::mda
  tp = function(model, newdata) predict(model, newdata)

  testCVParsets("classif.mda", multiclass.df, multiclass.target,
    tune.train = tt, tune.predict = tp, parset.list = parset.list1)
  testCV("classif.mda", multiclass.df, multiclass.target, tune.train = tt,
    tune.predict = tp, parset = list(start.method = "lvq", subclasses = 17))
})
