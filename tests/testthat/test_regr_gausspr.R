context("regr_gausspr")

test_that("regr_gausspr", {
  requirePackages("kernlab", default.method = "load")

  parset.list = list(
    list(),
    list(kernel = "splinedot"),
    list(var = 0.02)
  )

  old.predicts.list = list()

  for (i in seq_along(parset.list)) {
    parset = parset.list[[i]]
    pars = list(regr.formula, data = regr.train)
    pars = c(pars, parset)
    set.seed(getOption("mlr.debug.seed"))
    capture.output({
      m = do.call(kernlab::gausspr, pars)
    })
    p = kernlab::predict(m, newdata = regr.test)
    old.predicts.list[[i]] = p[, 1]
  }
  testSimpleParsets("regr.gausspr", regr.df, regr.target, regr.train.inds,
    old.predicts.list, parset.list)
})
