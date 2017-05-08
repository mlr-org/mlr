context("oneclass_ksvm")

test_that("oneclass_ksvm", {
  requirePackagesOrSkip("kernlab", default.method = "load")

  parset.list = list(
    list(),
    list(gamma = 20),
    list(kernel = "tanhdot", scale = 10),
    list(kernel = "polydot", degree = 3, offset = 2, scale = 1.5)
  )

  old.predicts.list = list()

  for (i in seq_along(parset.list)) {
    parset = parset.list[[i]]
    pars = list(x = oneclass.train[, -5])
    pars = c(pars, list(type = "one-classification"))
    pars = c(pars, parset)
    set.seed(getOption("mlr.debug.seed"))
    m1 = do.call(kernlab::ksvm, pars)
    old.predicts.list[[i]] = predict(m1, newdata = oneclass.test[, -5])
  }

   testSimpleParsets("oneclass.ksvm", oneclass.df, oneclass.target,
     oneclass.train.inds, old.predicts.list,  parset.list)

  tt = function(formula, data, subset=1:150, ...) {
    kernlab::ksvm(formula, data = data[subset, ], kernel = "polynomial", degree = 3, coef0 = 2, gamma = 1.5, type = "one-classification")
  }
})
