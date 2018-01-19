context("regr_ksvm")

test_that("regr_ksvm", {
  requirePackagesOrSkip("kernlab", default.method = "load")

  parset.list = list(
    list(),
    list(C = 0.3, kpar = list(sigma = 2)),
    list(C = 0.3, kpar = list(sigma = 2), epsilon = 0.3)
  )
  parset.list2 = list(
    list(),
    list(C = 0.3, sigma = 2),
    list(C = 0.3, sigma = 2, epsilon = 0.3)
  )

  old.predicts.list = list()

  for (i in seq_along(parset.list)) {
    parset = parset.list[[i]]
    pars = list(regr.formula, data = regr.train)
    pars = c(pars, parset)
    set.seed(getOption("mlr.debug.seed"))
    capture.output({
      m = do.call(kernlab::ksvm, pars)
    })
    p = kernlab::predict(m, newdata = regr.test)
    old.predicts.list[[i]] = p[, 1]
  }
  testSimpleParsets("regr.ksvm", regr.df, regr.target, regr.train.inds,
    old.predicts.list, parset.list2)
})
