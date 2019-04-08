context("regr_svm")

test_that("regr_svm", {
  requirePackagesOrSkip("e1071", default.method = "load")

  parset.list = list(
    list(),
    list(kernel = "linear", epsilon = 0.2),
    list(type = "nu-regression"),
    list(type = "nu-regression", kernel = "radial", nu = 0.5)
  )

  old.predicts.list = list()
  old.probs.list = list()

  for (i in seq_along(parset.list)) {
    parset = parset.list[[i]]
    pars = list(formula = regr.formula, data = regr.train)
    pars = c(pars, parset)
    set.seed(getOption("mlr.debug.seed"))
    m = do.call(e1071::svm, pars)
    p  = predict(m, newdata = regr.test)
    old.predicts.list[[i]] = p
  }

  testSimpleParsets("regr.svm", regr.df, regr.target, regr.train.inds, old.predicts.list, parset.list)

  tt = e1071::svm
  tp = function(model, newdata) predict(model, newdata)

  testCVParsets("regr.svm", regr.df, regr.target, tune.train = tt, tune.predict = tp, parset.list = parset.list)
})
