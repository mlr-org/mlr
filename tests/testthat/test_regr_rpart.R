context("regr_rpart")

test_that("regr_rpart", {
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
    pars = list(formula = regr.formula, data = regr.train)
    pars = c(pars, parset)
    m = do.call(rpart::rpart, pars)
    p = predict(m, newdata = regr.test)
    old.predicts.list[[i]] = p
  }

  testSimpleParsets("regr.rpart", regr.df, regr.target, regr.train.inds,
    old.predicts.list, parset.list)

  tt = rpart::rpart
  tp = function(model, newdata) predict(model, newdata)

  testCVParsets("regr.rpart", regr.df, regr.target, tune.train = tt,
    tune.predict = tp, parset.list = parset.list)
})
