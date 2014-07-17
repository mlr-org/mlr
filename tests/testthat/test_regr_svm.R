context("regr_svm")

test_that("regr_svm", {
  library(e1071)
  parset.list = list(
    list(),
    list(kernel = "linear", epsilon = 0.2),
    list(type = "nu-regression"),
    list(type = "nu-regression", kernel = "radial", nu = 0.5)
  )

  old.predicts.list = list()
  old.probs.list = list()

  for (i in 1:length(parset.list)) {
    parset = parset.list[[i]]
    pars = list(formula = regr.formula, data = regr.train)
    pars = c(pars, parset)
    set.seed(getOption("mlr.debug.seed"))
    m = do.call(svm, pars)
    p  = predict(m, newdata = regr.test)
    old.predicts.list[[i]] = p
  }

  testSimpleParsets("regr.svm", regr.df, regr.target, regr.train.inds, old.predicts.list, parset.list)

  tt = "svm"
  tp = function(model, newdata) predict(model, newdata)

  testCVParsets("regr.svm", regr.df, regr.target, tune.train = tt, tune.predict = tp, parset.list = parset.list)
})

