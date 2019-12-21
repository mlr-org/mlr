context("classif_rpart")

test_that("classif_rpart", {
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
    pars = list(formula = multiclass.formula, data = multiclass.train)
    pars = c(pars, parset)
    m = do.call(rpart::rpart, pars)
    p = predict(m, newdata = multiclass.test, type = "class")
    p2 = predict(m, newdata = multiclass.test, type = "prob")
    old.predicts.list[[i]] = p
    old.probs.list[[i]] = p2
  }

  testSimpleParsets("classif.rpart", multiclass.df, multiclass.target,
    multiclass.train.inds, old.predicts.list, parset.list)
  testProbParsets("classif.rpart", multiclass.df, multiclass.target,
    multiclass.train.inds, old.probs.list, parset.list)

  tt = rpart::rpart
  tp = function(model, newdata) predict(model, newdata, type = "class")

  testCVParsets("classif.rpart", multiclass.df, multiclass.target,
    tune.train = tt, tune.predict = tp, parset.list = parset.list)
})
